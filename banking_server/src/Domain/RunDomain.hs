module Domain.RunDomain where

-- import Domain.Account
-- import Data.UUID.V4 (nextRandom)
-- import Domain.Customer (CustomerId(CustomerId))

-- uuid <- nextRandom

-- let cid = CustomerId uuid
--     i   = Iban "AT12 12345 01234567890"
--     a0  = account cid i

-- (a1, (ret, es)) <- execCommand a0 (Deposit 100)
-- putStrLn $ "Account emitted DomainEvents: " ++ show es
-- putStrLn $ "Account returned: " ++ show ret

-- (_a2, (ret', es')) <- execCommand a1 (Withdraw 2000)
-- putStrLn $ "Account emitted DomainEvents: " ++ show es'
-- putStrLn $ "Account returned: " ++ show ret'

-- let cmds = 
--       [ Deposit 100
--       , Withdraw 2000
--       , TransferTo i 1000 "Jonathan" "Rent" 
--       , ReceiveFrom i 1000 "Jonathan" "Rent" ]

-- (rets, es) <- execCommands a0 cmds
-- putStrLn $ "Account emitted DomainEvents: " ++ show es
-- putStrLn $ "Account returned: " ++ show rets