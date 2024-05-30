(namespace (read-msg 'ns))

(module conventional-auction GOVERNANCE
  @doc "Conventional auction contract"

  (defconst ADMIN-KS:string "marmalade-sale.marmalade-contract-admin")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (use marmalade-v2.policy-manager)
  (use marmalade-v2.policy-manager [BUYER-FUNGIBLE-ACCOUNT-MSG-KEY MARKETPLACE-FEE-KEY marketplace-fee-spec])
  (use marmalade-v2.util-v1)
  (implements marmalade-v2.sale-v2)

  (defschema auctions-schema
    token-id:string
    start-date:integer
    end-date:integer
    highest-bid:decimal
    highest-bid-id:string
    reserve-price:decimal
  )

  (defschema bids-schema
    bidder:string
    bidder-guard:guard
    bid:decimal
  )

  (deftable auctions:{auctions-schema})
  (deftable bids:{bids-schema})
  (deftable mk-fees:{marketplace-fee-spec})

  (defcap AUCTION_CREATED:bool
    ( sale-id:string
      token-id:string
      escrow:string
    )
    @event
    true
  )

  (defcap MANAGE_AUCTION:bool (sale-id:string token-id:string)
    (let* (
      (quote-info:object{quote-schema} (get-quote-info sale-id))
      (seller:string (at 'seller quote-info)))
      (enforce-guard (marmalade-v2.ledger.account-guard token-id seller))
    )
  )

  (defcap BID_PLACED:bool
    ( bid-id:string
      bidder:string
      bidder-guard:guard
      bid:decimal
      token-id:string
    )
    @event
    true
  )

  (defcap PLACE_BID:bool (bidder-guard:guard)
    (enforce-guard bidder-guard)
  )

  (defcap REFUND_CAP:bool (sale-id:string) true)

  (defun escrow-account:string (sale-id:string)
    (create-principal (escrow-guard sale-id))
  )

  (defun escrow-guard(sale-id:string)
    (util.guards1.guard-any [
      (create-capability-guard (REFUND_CAP sale-id))
      (create-user-guard (enforce-fungible-transfer sale-id))
    ])
  )

  (defun enforce-fungible-transfer:bool (sale-id:string)
    (require-capability (FUNGIBLE-TRANSFER-CALL sale-id) )
  )

  (defun enforce-quote-update:bool (sale-id:string price:decimal)
    (require-capability (SALE-GUARD-CALL sale-id price))
    (with-read auctions sale-id
      { 'token-id:= token-id,
        'start-date:= start-date,
        'end-date:= end-date,
        'highest-bid:= highest-bid,
        'highest-bid-id:= highest-bid-id
      }
      (enforce (> (curr-time) end-date) "Auction is still ongoing")
      (enforce (> highest-bid 0.0) "No bids have been placed")
      (enforce (= price highest-bid) "Price does not match highest bid")
      (enforce (= (read-msg BUYER-FUNGIBLE-ACCOUNT-MSG-KEY) (escrow-account sale-id)) "Buyer fungible account must match escrow account")
      (with-read bids highest-bid-id
        { 'bidder:= bidder,
          'bidder-guard:= bidder-guard,
          'bid:= bid
        }
        (enforce (= (read-msg "buyer") bidder) "Buyer does not match highest bidder")
        (enforce (= (read-msg "buyer-guard" ) bidder-guard) "Buyer-guard does not match highest bidder-guard")
      )
      (with-default-read mk-fees highest-bid-id
        { 'mk-account: "", 'mk-fee-percentage: 0.0 }
        { 'mk-account:= mk-account,
          'mk-fee-percentage:= mk-fee-percentage
        }
        (if (= mk-fee-percentage 0.0)
          true
          (let ((mk-fee-spec:object{marketplace-fee-spec} (try { "mk-account": "", "mk-fee-percentage": 0.0 } (read-msg MARKETPLACE-FEE-KEY))))
            (enforce (= mk-account (at "mk-account" mk-fee-spec)) "Marketplace fee account does not match stored marketplace fee account")
            (enforce (= mk-fee-percentage (at "mk-fee-percentage" mk-fee-spec)) "Marketplace fee percentage does not match stored marketplace fee percentage")
          )
        )
      )
    )
    true
  )

  (defun enforce-withdrawal:bool (sale-id:string)
    (with-read auctions sale-id
      { 'end-date:= end-date,
        'highest-bid:= highest-bid
      }
      (enforce (> (curr-time) end-date) "Auction is still ongoing or hasn't started yet")
      (enforce (= highest-bid 0.0) "Bid has been placed, can't withdraw")
    )
    true
  )

  (defun create-bid-id:string (sale-id:string bidder:string)
    (hash [sale-id bidder (int-to-str 10 (curr-time))]))

  (defun create-auction
    ( sale-id:string
      token-id:string
      start-date:integer
      end-date:integer
      reserve-price:decimal
    )
    (enforce (> start-date (curr-time)) "Start date must be in the future")
    (enforce (> end-date start-date) "End date must be after start date")
    (enforce (> reserve-price 0.0) "Reserve price must be greater than 0")
    (let (
      (quote-info:object{quote-schema} (get-quote-info sale-id)))

      (enforce (= (at 'sale-price quote-info) 0.0) "Quote price must be 0")
      (enforce (= (at 'sale-type quote-info) "marmalade-sale.conventional-auction") "Quote does not support auction")
      (enforce (= token-id (at 'token-id quote-info)) "Token-id does not match quote token-id")
    )

    (with-capability (MANAGE_AUCTION sale-id token-id)
      (insert auctions sale-id {
        "token-id": token-id
        ,"start-date": start-date
        ,"end-date": end-date
        ,"highest-bid": 0.0
        ,"highest-bid-id": ""
        ,"reserve-price": reserve-price
      })
      (emit-event (AUCTION_CREATED sale-id token-id (escrow-account sale-id)))
    )
  )

  (defun update-auction
    ( sale-id:string
      start-date:integer
      end-date:integer
      reserve-price:decimal
    )
    (enforce (> start-date (curr-time)) "Start date must be in the future")
    (enforce (> end-date start-date) "End date must be after start date")
    (enforce (> reserve-price 0.0) "Reserve price must be greater than 0")

    (with-read auctions sale-id
      { 'token-id:= token-id,
        'start-date:= curr-start-date
      }
      (enforce (> curr-start-date (curr-time)) "Can't update auction after it has started")

      (with-capability (MANAGE_AUCTION sale-id token-id)
        (update auctions sale-id {
          "start-date": start-date
          ,"end-date": end-date
          ,"reserve-price": reserve-price
        })
      ))
  )

  (defun retrieve-auction (sale-id:string)
    (read auctions sale-id)
  )

  (defun retrieve-bid (bid-id:string)
    (read bids bid-id)
  )

  (defun place-bid:bool
    ( sale-id:string
      bidder:string
      bidder-guard:guard
      bid:decimal)
    @model [
      (property (is-principal bidder))
    ]
    (with-read auctions sale-id
      { 'token-id:= token-id,
        'start-date:= start-date,
        'end-date:= end-date,
        'highest-bid:= prev-highest-bid,
        'highest-bid-id:= prev-highest-bid-id,
        'reserve-price:= reserve-price
      }
      (enforce (> (curr-time) start-date) "Auction has not started yet")
      (enforce (< (curr-time) end-date) "Auction has ended")
      (enforce (> bid prev-highest-bid) "Bid is not higher than previous highest bid")
      (enforce (> bid reserve-price) "Bid is not higher than reserve price")
      (enforce (validate-principal bidder-guard bidder) "Incorrect account guard, only principal accounts allowed")
      (let* (
          (bid-id:string (create-bid-id sale-id bidder))
          (quote-info:object{quote-schema} (get-quote-info sale-id))
          (fungible:module{fungible-v2} (at 'fungible quote-info))
          (mk-fee-spec:object{marketplace-fee-spec} (try { "mk-account": "", "mk-fee-percentage": 0.0 } (read-msg MARKETPLACE-FEE-KEY)))
          (mk-fee-percentage:decimal (at "mk-fee-percentage" mk-fee-spec))
          (mk-fee:decimal (floor (* mk-fee-percentage bid) (fungible::precision)))
        )
        (enforce (and (>= mk-fee-percentage 0.0) (<= mk-fee-percentage 1.0)) "Invalid market-fee percentage")
        (with-capability (PLACE_BID bidder-guard)
          ; Return amount to previous bidder if there was one
          (if (> prev-highest-bid 0.0)
            (with-read bids prev-highest-bid-id
              { 'bidder:= previous-bidder,
                'bidder-guard:= previous-bidder-guard
              }
              (with-capability (REFUND_CAP sale-id)
                (install-capability (fungible::TRANSFER (escrow-account sale-id) previous-bidder (fungible::get-balance (escrow-account sale-id))))
                (fungible::transfer (escrow-account sale-id) previous-bidder (fungible::get-balance (escrow-account sale-id)))
              )
            )
            true
          )

          ; Transfer amount (including fee if applicable) from bidder to escrow-account
          (fungible::transfer-create bidder (escrow-account sale-id) (escrow-guard sale-id) (+ bid mk-fee))

          ; Write new bid and store highest bid in auction
          (write bids bid-id {
            "bidder": bidder
            ,"bidder-guard": bidder-guard
            ,"bid": bid
          })

          ; Store marketplace fee configuration
          (if (= mk-fee-percentage 0.0)
            true
            (let (
             (mk-account:string (at 'account (fungible::details (at 'mk-account mk-fee-spec)))))
             (enforce (!= "" mk-account) "Marketplace fee account does not exist")
              (write mk-fees bid-id mk-fee-spec)
            )
          )
          (update auctions sale-id { 'highest-bid: bid, 'highest-bid-id: bid-id })
          (emit-event (BID_PLACED bid-id bidder bidder-guard bid token-id))
        ))
      true
    )
  )
)

(if (read-msg "upgrade")
  ["upgrade complete"]
  [
    (create-table mk-fees)
    (create-table auctions)
    (create-table bids)
  ]
)
(enforce-guard ADMIN-KS)
