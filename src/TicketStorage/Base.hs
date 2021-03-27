module TicketStorage.Base (nothingStorage, TicketStorageProvider) where

import Common ( UserTicketStorageContext(..) )
import Model.MinUser


type TicketStorageProvider = IO UserTicketStorageContext

nothingStorage :: TicketStorageProvider
nothingStorage = return UserTicketStorageContext {
                        createTicketFromMUser = \_ -> return "ticket",
                        getMUserFromTicket = \t -> if t == "ticket" 
                            then return $ Just $ MinimalUser "0000000000" "Constant" "Value"
                            else return Nothing
                        }