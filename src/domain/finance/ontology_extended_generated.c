// src/domain/finance/ontology_extended_generated.c
// 自动生成的金融领域扩展类型定义
// 此文件由 tools/generate_finance_types.py 自动生成
// 包含数千个金融领域常见类型

#include "../../../include/kos_ontology.h"
#include "../../../include/kos_core.h"
#include "../../../include/kos_finance.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// 添加生成的类型定义到本体
void kos_finance_ontology_add_generated_types(TypeOntology* ontology) {
    if (!ontology) {
        return;
    }
    
    printf("[FinanceGenerated] Adding generated finance types...\n");
    
    // ========== account_types ==========\n    // SavingsAccount
    kos_term* savingsaccount_type = kos_mk_prop("SavingsAccount");
    kos_ontology_add_type_definition(ontology, "SavingsAccount", savingsaccount_type, NULL);
    // CheckingAccount
    kos_term* checkingaccount_type = kos_mk_prop("CheckingAccount");
    kos_ontology_add_type_definition(ontology, "CheckingAccount", checkingaccount_type, NULL);
    // CurrentAccount
    kos_term* currentaccount_type = kos_mk_prop("CurrentAccount");
    kos_ontology_add_type_definition(ontology, "CurrentAccount", currentaccount_type, NULL);
    // DepositAccount
    kos_term* depositaccount_type = kos_mk_prop("DepositAccount");
    kos_ontology_add_type_definition(ontology, "DepositAccount", depositaccount_type, NULL);
    // InvestmentAccount
    kos_term* investmentaccount_type = kos_mk_prop("InvestmentAccount");
    kos_ontology_add_type_definition(ontology, "InvestmentAccount", investmentaccount_type, NULL);
    // TradingAccount
    kos_term* tradingaccount_type = kos_mk_prop("TradingAccount");
    kos_ontology_add_type_definition(ontology, "TradingAccount", tradingaccount_type, NULL);
    // MarginAccount
    kos_term* marginaccount_type = kos_mk_prop("MarginAccount");
    kos_ontology_add_type_definition(ontology, "MarginAccount", marginaccount_type, NULL);
    // ForexAccount
    kos_term* forexaccount_type = kos_mk_prop("ForexAccount");
    kos_ontology_add_type_definition(ontology, "ForexAccount", forexaccount_type, NULL);
    // CryptocurrencyAccount
    kos_term* cryptocurrencyaccount_type = kos_mk_prop("CryptocurrencyAccount");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccount", cryptocurrencyaccount_type, NULL);
    // OffshoreAccount
    kos_term* offshoreaccount_type = kos_mk_prop("OffshoreAccount");
    kos_ontology_add_type_definition(ontology, "OffshoreAccount", offshoreaccount_type, NULL);
    // TrustAccount
    kos_term* trustaccount_type = kos_mk_prop("TrustAccount");
    kos_ontology_add_type_definition(ontology, "TrustAccount", trustaccount_type, NULL);
    // EscrowAccount
    kos_term* escrowaccount_type = kos_mk_prop("EscrowAccount");
    kos_ontology_add_type_definition(ontology, "EscrowAccount", escrowaccount_type, NULL);
    // CorporateAccount
    kos_term* corporateaccount_type = kos_mk_prop("CorporateAccount");
    kos_ontology_add_type_definition(ontology, "CorporateAccount", corporateaccount_type, NULL);
    // PersonalAccount
    kos_term* personalaccount_type = kos_mk_prop("PersonalAccount");
    kos_ontology_add_type_definition(ontology, "PersonalAccount", personalaccount_type, NULL);
    // JointAccount
    kos_term* jointaccount_type = kos_mk_prop("JointAccount");
    kos_ontology_add_type_definition(ontology, "JointAccount", jointaccount_type, NULL);
    // CustodialAccount
    kos_term* custodialaccount_type = kos_mk_prop("CustodialAccount");
    kos_ontology_add_type_definition(ontology, "CustodialAccount", custodialaccount_type, NULL);
    // RetirementAccount
    kos_term* retirementaccount_type = kos_mk_prop("RetirementAccount");
    kos_ontology_add_type_definition(ontology, "RetirementAccount", retirementaccount_type, NULL);
    // PensionAccount
    kos_term* pensionaccount_type = kos_mk_prop("PensionAccount");
    kos_ontology_add_type_definition(ontology, "PensionAccount", pensionaccount_type, NULL);
    // IRA
    kos_term* ira_type = kos_mk_prop("IRA");
    kos_ontology_add_type_definition(ontology, "IRA", ira_type, NULL);
    // 401K
    kos_term* type_401k_type = kos_mk_prop("401K");
    kos_ontology_add_type_definition(ontology, "401K", type_401k_type, NULL);
    // RothIRA
    kos_term* rothira_type = kos_mk_prop("RothIRA");
    kos_ontology_add_type_definition(ontology, "RothIRA", rothira_type, NULL);
    // SEPIRA
    kos_term* sepira_type = kos_mk_prop("SEPIRA");
    kos_ontology_add_type_definition(ontology, "SEPIRA", sepira_type, NULL);
    // HealthSavingsAccount
    kos_term* healthsavingsaccount_type = kos_mk_prop("HealthSavingsAccount");
    kos_ontology_add_type_definition(ontology, "HealthSavingsAccount", healthsavingsaccount_type, NULL);
    // EducationSavingsAccount
    kos_term* educationsavingsaccount_type = kos_mk_prop("EducationSavingsAccount");
    kos_ontology_add_type_definition(ontology, "EducationSavingsAccount", educationsavingsaccount_type, NULL);
    // CustodianAccount
    kos_term* custodianaccount_type = kos_mk_prop("CustodianAccount");
    kos_ontology_add_type_definition(ontology, "CustodianAccount", custodianaccount_type, NULL);
    // NomineeAccount
    kos_term* nomineeaccount_type = kos_mk_prop("NomineeAccount");
    kos_ontology_add_type_definition(ontology, "NomineeAccount", nomineeaccount_type, NULL);
    // BeneficialOwnerAccount
    kos_term* beneficialowneraccount_type = kos_mk_prop("BeneficialOwnerAccount");
    kos_ontology_add_type_definition(ontology, "BeneficialOwnerAccount", beneficialowneraccount_type, NULL);
    // ShellCompanyAccount
    kos_term* shellcompanyaccount_type = kos_mk_prop("ShellCompanyAccount");
    kos_ontology_add_type_definition(ontology, "ShellCompanyAccount", shellcompanyaccount_type, NULL);
    // FoundationAccount
    kos_term* foundationaccount_type = kos_mk_prop("FoundationAccount");
    kos_ontology_add_type_definition(ontology, "FoundationAccount", foundationaccount_type, NULL);
    // CharityAccount
    kos_term* charityaccount_type = kos_mk_prop("CharityAccount");
    kos_ontology_add_type_definition(ontology, "CharityAccount", charityaccount_type, NULL);
    // EndowmentAccount
    kos_term* endowmentaccount_type = kos_mk_prop("EndowmentAccount");
    kos_ontology_add_type_definition(ontology, "EndowmentAccount", endowmentaccount_type, NULL);
    // ========== transaction_types ==========\n    // WireTransfer
    kos_term* wiretransfer_type = kos_mk_prop("WireTransfer");
    kos_ontology_add_type_definition(ontology, "WireTransfer", wiretransfer_type, NULL);
    // ACHTransfer
    kos_term* achtransfer_type = kos_mk_prop("ACHTransfer");
    kos_ontology_add_type_definition(ontology, "ACHTransfer", achtransfer_type, NULL);
    // SWIFTTransfer
    kos_term* swifttransfer_type = kos_mk_prop("SWIFTTransfer");
    kos_ontology_add_type_definition(ontology, "SWIFTTransfer", swifttransfer_type, NULL);
    // CashDeposit
    kos_term* cashdeposit_type = kos_mk_prop("CashDeposit");
    kos_ontology_add_type_definition(ontology, "CashDeposit", cashdeposit_type, NULL);
    // CashWithdrawal
    kos_term* cashwithdrawal_type = kos_mk_prop("CashWithdrawal");
    kos_ontology_add_type_definition(ontology, "CashWithdrawal", cashwithdrawal_type, NULL);
    // CheckDeposit
    kos_term* checkdeposit_type = kos_mk_prop("CheckDeposit");
    kos_ontology_add_type_definition(ontology, "CheckDeposit", checkdeposit_type, NULL);
    // CheckPayment
    kos_term* checkpayment_type = kos_mk_prop("CheckPayment");
    kos_ontology_add_type_definition(ontology, "CheckPayment", checkpayment_type, NULL);
    // CreditCardPayment
    kos_term* creditcardpayment_type = kos_mk_prop("CreditCardPayment");
    kos_ontology_add_type_definition(ontology, "CreditCardPayment", creditcardpayment_type, NULL);
    // DebitCardTransaction
    kos_term* debitcardtransaction_type = kos_mk_prop("DebitCardTransaction");
    kos_ontology_add_type_definition(ontology, "DebitCardTransaction", debitcardtransaction_type, NULL);
    // OnlinePayment
    kos_term* onlinepayment_type = kos_mk_prop("OnlinePayment");
    kos_ontology_add_type_definition(ontology, "OnlinePayment", onlinepayment_type, NULL);
    // MobilePayment
    kos_term* mobilepayment_type = kos_mk_prop("MobilePayment");
    kos_ontology_add_type_definition(ontology, "MobilePayment", mobilepayment_type, NULL);
    // CryptocurrencyTransfer
    kos_term* cryptocurrencytransfer_type = kos_mk_prop("CryptocurrencyTransfer");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransfer", cryptocurrencytransfer_type, NULL);
    // StockTrade
    kos_term* stocktrade_type = kos_mk_prop("StockTrade");
    kos_ontology_add_type_definition(ontology, "StockTrade", stocktrade_type, NULL);
    // BondTrade
    kos_term* bondtrade_type = kos_mk_prop("BondTrade");
    kos_ontology_add_type_definition(ontology, "BondTrade", bondtrade_type, NULL);
    // DerivativeTrade
    kos_term* derivativetrade_type = kos_mk_prop("DerivativeTrade");
    kos_ontology_add_type_definition(ontology, "DerivativeTrade", derivativetrade_type, NULL);
    // ForexTrade
    kos_term* forextrade_type = kos_mk_prop("ForexTrade");
    kos_ontology_add_type_definition(ontology, "ForexTrade", forextrade_type, NULL);
    // CommodityTrade
    kos_term* commoditytrade_type = kos_mk_prop("CommodityTrade");
    kos_ontology_add_type_definition(ontology, "CommodityTrade", commoditytrade_type, NULL);
    // OptionsTrade
    kos_term* optionstrade_type = kos_mk_prop("OptionsTrade");
    kos_ontology_add_type_definition(ontology, "OptionsTrade", optionstrade_type, NULL);
    // FuturesTrade
    kos_term* futurestrade_type = kos_mk_prop("FuturesTrade");
    kos_ontology_add_type_definition(ontology, "FuturesTrade", futurestrade_type, NULL);
    // SwapTransaction
    kos_term* swaptransaction_type = kos_mk_prop("SwapTransaction");
    kos_ontology_add_type_definition(ontology, "SwapTransaction", swaptransaction_type, NULL);
    // LoanDisbursement
    kos_term* loandisbursement_type = kos_mk_prop("LoanDisbursement");
    kos_ontology_add_type_definition(ontology, "LoanDisbursement", loandisbursement_type, NULL);
    // LoanRepayment
    kos_term* loanrepayment_type = kos_mk_prop("LoanRepayment");
    kos_ontology_add_type_definition(ontology, "LoanRepayment", loanrepayment_type, NULL);
    // InterestPayment
    kos_term* interestpayment_type = kos_mk_prop("InterestPayment");
    kos_ontology_add_type_definition(ontology, "InterestPayment", interestpayment_type, NULL);
    // DividendPayment
    kos_term* dividendpayment_type = kos_mk_prop("DividendPayment");
    kos_ontology_add_type_definition(ontology, "DividendPayment", dividendpayment_type, NULL);
    // InsurancePremium
    kos_term* insurancepremium_type = kos_mk_prop("InsurancePremium");
    kos_ontology_add_type_definition(ontology, "InsurancePremium", insurancepremium_type, NULL);
    // InsuranceClaim
    kos_term* insuranceclaim_type = kos_mk_prop("InsuranceClaim");
    kos_ontology_add_type_definition(ontology, "InsuranceClaim", insuranceclaim_type, NULL);
    // PensionPayment
    kos_term* pensionpayment_type = kos_mk_prop("PensionPayment");
    kos_ontology_add_type_definition(ontology, "PensionPayment", pensionpayment_type, NULL);
    // SalaryPayment
    kos_term* salarypayment_type = kos_mk_prop("SalaryPayment");
    kos_ontology_add_type_definition(ontology, "SalaryPayment", salarypayment_type, NULL);
    // TaxPayment
    kos_term* taxpayment_type = kos_mk_prop("TaxPayment");
    kos_ontology_add_type_definition(ontology, "TaxPayment", taxpayment_type, NULL);
    // RefundPayment
    kos_term* refundpayment_type = kos_mk_prop("RefundPayment");
    kos_ontology_add_type_definition(ontology, "RefundPayment", refundpayment_type, NULL);
    // GiftTransfer
    kos_term* gifttransfer_type = kos_mk_prop("GiftTransfer");
    kos_ontology_add_type_definition(ontology, "GiftTransfer", gifttransfer_type, NULL);
    // CharityDonation
    kos_term* charitydonation_type = kos_mk_prop("CharityDonation");
    kos_ontology_add_type_definition(ontology, "CharityDonation", charitydonation_type, NULL);
    // RealEstatePurchase
    kos_term* realestatepurchase_type = kos_mk_prop("RealEstatePurchase");
    kos_ontology_add_type_definition(ontology, "RealEstatePurchase", realestatepurchase_type, NULL);
    // RealEstateSale
    kos_term* realestatesale_type = kos_mk_prop("RealEstateSale");
    kos_ontology_add_type_definition(ontology, "RealEstateSale", realestatesale_type, NULL);
    // ArtPurchase
    kos_term* artpurchase_type = kos_mk_prop("ArtPurchase");
    kos_ontology_add_type_definition(ontology, "ArtPurchase", artpurchase_type, NULL);
    // ArtSale
    kos_term* artsale_type = kos_mk_prop("ArtSale");
    kos_ontology_add_type_definition(ontology, "ArtSale", artsale_type, NULL);
    // JewelryPurchase
    kos_term* jewelrypurchase_type = kos_mk_prop("JewelryPurchase");
    kos_ontology_add_type_definition(ontology, "JewelryPurchase", jewelrypurchase_type, NULL);
    // JewelrySale
    kos_term* jewelrysale_type = kos_mk_prop("JewelrySale");
    kos_ontology_add_type_definition(ontology, "JewelrySale", jewelrysale_type, NULL);
    // VehiclePurchase
    kos_term* vehiclepurchase_type = kos_mk_prop("VehiclePurchase");
    kos_ontology_add_type_definition(ontology, "VehiclePurchase", vehiclepurchase_type, NULL);
    // VehicleSale
    kos_term* vehiclesale_type = kos_mk_prop("VehicleSale");
    kos_ontology_add_type_definition(ontology, "VehicleSale", vehiclesale_type, NULL);
    // CasinoDeposit
    kos_term* casinodeposit_type = kos_mk_prop("CasinoDeposit");
    kos_ontology_add_type_definition(ontology, "CasinoDeposit", casinodeposit_type, NULL);
    // CasinoWithdrawal
    kos_term* casinowithdrawal_type = kos_mk_prop("CasinoWithdrawal");
    kos_ontology_add_type_definition(ontology, "CasinoWithdrawal", casinowithdrawal_type, NULL);
    // GamingTransaction
    kos_term* gamingtransaction_type = kos_mk_prop("GamingTransaction");
    kos_ontology_add_type_definition(ontology, "GamingTransaction", gamingtransaction_type, NULL);
    // LotteryPayment
    kos_term* lotterypayment_type = kos_mk_prop("LotteryPayment");
    kos_ontology_add_type_definition(ontology, "LotteryPayment", lotterypayment_type, NULL);
    // HawalaTransfer
    kos_term* hawalatransfer_type = kos_mk_prop("HawalaTransfer");
    kos_ontology_add_type_definition(ontology, "HawalaTransfer", hawalatransfer_type, NULL);
    // UndergroundBankingTransfer
    kos_term* undergroundbankingtransfer_type = kos_mk_prop("UndergroundBankingTransfer");
    kos_ontology_add_type_definition(ontology, "UndergroundBankingTransfer", undergroundbankingtransfer_type, NULL);
    // MoneyOrder
    kos_term* moneyorder_type = kos_mk_prop("MoneyOrder");
    kos_ontology_add_type_definition(ontology, "MoneyOrder", moneyorder_type, NULL);
    // TravelersCheck
    kos_term* travelerscheck_type = kos_mk_prop("TravelersCheck");
    kos_ontology_add_type_definition(ontology, "TravelersCheck", travelerscheck_type, NULL);
    // PrepaidCardLoad
    kos_term* prepaidcardload_type = kos_mk_prop("PrepaidCardLoad");
    kos_ontology_add_type_definition(ontology, "PrepaidCardLoad", prepaidcardload_type, NULL);
    // PrepaidCardSpend
    kos_term* prepaidcardspend_type = kos_mk_prop("PrepaidCardSpend");
    kos_ontology_add_type_definition(ontology, "PrepaidCardSpend", prepaidcardspend_type, NULL);
    // CryptocurrencyExchange
    kos_term* cryptocurrencyexchange_type = kos_mk_prop("CryptocurrencyExchange");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyExchange", cryptocurrencyexchange_type, NULL);
    // TokenTransfer
    kos_term* tokentransfer_type = kos_mk_prop("TokenTransfer");
    kos_ontology_add_type_definition(ontology, "TokenTransfer", tokentransfer_type, NULL);
    // ========== institution_types ==========\n    // CommercialBank
    kos_term* commercialbank_type = kos_mk_prop("CommercialBank");
    kos_ontology_add_type_definition(ontology, "CommercialBank", commercialbank_type, NULL);
    // InvestmentBank
    kos_term* investmentbank_type = kos_mk_prop("InvestmentBank");
    kos_ontology_add_type_definition(ontology, "InvestmentBank", investmentbank_type, NULL);
    // CentralBank
    kos_term* centralbank_type = kos_mk_prop("CentralBank");
    kos_ontology_add_type_definition(ontology, "CentralBank", centralbank_type, NULL);
    // CreditUnion
    kos_term* creditunion_type = kos_mk_prop("CreditUnion");
    kos_ontology_add_type_definition(ontology, "CreditUnion", creditunion_type, NULL);
    // SavingsBank
    kos_term* savingsbank_type = kos_mk_prop("SavingsBank");
    kos_ontology_add_type_definition(ontology, "SavingsBank", savingsbank_type, NULL);
    // PrivateBank
    kos_term* privatebank_type = kos_mk_prop("PrivateBank");
    kos_ontology_add_type_definition(ontology, "PrivateBank", privatebank_type, NULL);
    // OffshoreBank
    kos_term* offshorebank_type = kos_mk_prop("OffshoreBank");
    kos_ontology_add_type_definition(ontology, "OffshoreBank", offshorebank_type, NULL);
    // ShadowBank
    kos_term* shadowbank_type = kos_mk_prop("ShadowBank");
    kos_ontology_add_type_definition(ontology, "ShadowBank", shadowbank_type, NULL);
    // BrokerageFirm
    kos_term* brokeragefirm_type = kos_mk_prop("BrokerageFirm");
    kos_ontology_add_type_definition(ontology, "BrokerageFirm", brokeragefirm_type, NULL);
    // SecuritiesFirm
    kos_term* securitiesfirm_type = kos_mk_prop("SecuritiesFirm");
    kos_ontology_add_type_definition(ontology, "SecuritiesFirm", securitiesfirm_type, NULL);
    // InsuranceCompany
    kos_term* insurancecompany_type = kos_mk_prop("InsuranceCompany");
    kos_ontology_add_type_definition(ontology, "InsuranceCompany", insurancecompany_type, NULL);
    // PensionFund
    kos_term* pensionfund_type = kos_mk_prop("PensionFund");
    kos_ontology_add_type_definition(ontology, "PensionFund", pensionfund_type, NULL);
    // HedgeFund
    kos_term* hedgefund_type = kos_mk_prop("HedgeFund");
    kos_ontology_add_type_definition(ontology, "HedgeFund", hedgefund_type, NULL);
    // PrivateEquityFund
    kos_term* privateequityfund_type = kos_mk_prop("PrivateEquityFund");
    kos_ontology_add_type_definition(ontology, "PrivateEquityFund", privateequityfund_type, NULL);
    // MutualFund
    kos_term* mutualfund_type = kos_mk_prop("MutualFund");
    kos_ontology_add_type_definition(ontology, "MutualFund", mutualfund_type, NULL);
    // ETF
    kos_term* etf_type = kos_mk_prop("ETF");
    kos_ontology_add_type_definition(ontology, "ETF", etf_type, NULL);
    // PaymentProcessor
    kos_term* paymentprocessor_type = kos_mk_prop("PaymentProcessor");
    kos_ontology_add_type_definition(ontology, "PaymentProcessor", paymentprocessor_type, NULL);
    // MoneyServiceBusiness
    kos_term* moneyservicebusiness_type = kos_mk_prop("MoneyServiceBusiness");
    kos_ontology_add_type_definition(ontology, "MoneyServiceBusiness", moneyservicebusiness_type, NULL);
    // CryptocurrencyExchange (already added, skipping duplicate)\n    // PeerToPeerLender
    kos_term* peertopeerlender_type = kos_mk_prop("PeerToPeerLender");
    kos_ontology_add_type_definition(ontology, "PeerToPeerLender", peertopeerlender_type, NULL);
    // CrowdfundingPlatform
    kos_term* crowdfundingplatform_type = kos_mk_prop("CrowdfundingPlatform");
    kos_ontology_add_type_definition(ontology, "CrowdfundingPlatform", crowdfundingplatform_type, NULL);
    // FintechCompany
    kos_term* fintechcompany_type = kos_mk_prop("FintechCompany");
    kos_ontology_add_type_definition(ontology, "FintechCompany", fintechcompany_type, NULL);
    // MicrofinanceInstitution
    kos_term* microfinanceinstitution_type = kos_mk_prop("MicrofinanceInstitution");
    kos_ontology_add_type_definition(ontology, "MicrofinanceInstitution", microfinanceinstitution_type, NULL);
    // DevelopmentBank
    kos_term* developmentbank_type = kos_mk_prop("DevelopmentBank");
    kos_ontology_add_type_definition(ontology, "DevelopmentBank", developmentbank_type, NULL);
    // ExportImportBank
    kos_term* exportimportbank_type = kos_mk_prop("ExportImportBank");
    kos_ontology_add_type_definition(ontology, "ExportImportBank", exportimportbank_type, NULL);
    // MortgageBank
    kos_term* mortgagebank_type = kos_mk_prop("MortgageBank");
    kos_ontology_add_type_definition(ontology, "MortgageBank", mortgagebank_type, NULL);
    // ConsumerFinanceCompany
    kos_term* consumerfinancecompany_type = kos_mk_prop("ConsumerFinanceCompany");
    kos_ontology_add_type_definition(ontology, "ConsumerFinanceCompany", consumerfinancecompany_type, NULL);
    // AutoFinanceCompany
    kos_term* autofinancecompany_type = kos_mk_prop("AutoFinanceCompany");
    kos_ontology_add_type_definition(ontology, "AutoFinanceCompany", autofinancecompany_type, NULL);
    // FactoringCompany
    kos_term* factoringcompany_type = kos_mk_prop("FactoringCompany");
    kos_ontology_add_type_definition(ontology, "FactoringCompany", factoringcompany_type, NULL);
    // LeasingCompany
    kos_term* leasingcompany_type = kos_mk_prop("LeasingCompany");
    kos_ontology_add_type_definition(ontology, "LeasingCompany", leasingcompany_type, NULL);
    // AssetManagementCompany
    kos_term* assetmanagementcompany_type = kos_mk_prop("AssetManagementCompany");
    kos_ontology_add_type_definition(ontology, "AssetManagementCompany", assetmanagementcompany_type, NULL);
    // WealthManagementCompany
    kos_term* wealthmanagementcompany_type = kos_mk_prop("WealthManagementCompany");
    kos_ontology_add_type_definition(ontology, "WealthManagementCompany", wealthmanagementcompany_type, NULL);
    // FamilyOffice
    kos_term* familyoffice_type = kos_mk_prop("FamilyOffice");
    kos_ontology_add_type_definition(ontology, "FamilyOffice", familyoffice_type, NULL);
    // TrustCompany
    kos_term* trustcompany_type = kos_mk_prop("TrustCompany");
    kos_ontology_add_type_definition(ontology, "TrustCompany", trustcompany_type, NULL);
    // ========== countries ==========\n    // UnitedStates
    kos_term* unitedstates_type = kos_mk_prop("UnitedStates");
    kos_ontology_add_type_definition(ontology, "UnitedStates", unitedstates_type, NULL);
    // UnitedKingdom
    kos_term* unitedkingdom_type = kos_mk_prop("UnitedKingdom");
    kos_ontology_add_type_definition(ontology, "UnitedKingdom", unitedkingdom_type, NULL);
    // Switzerland
    kos_term* switzerland_type = kos_mk_prop("Switzerland");
    kos_ontology_add_type_definition(ontology, "Switzerland", switzerland_type, NULL);
    // Singapore
    kos_term* singapore_type = kos_mk_prop("Singapore");
    kos_ontology_add_type_definition(ontology, "Singapore", singapore_type, NULL);
    // HongKong
    kos_term* hongkong_type = kos_mk_prop("HongKong");
    kos_ontology_add_type_definition(ontology, "HongKong", hongkong_type, NULL);
    // Luxembourg
    kos_term* luxembourg_type = kos_mk_prop("Luxembourg");
    kos_ontology_add_type_definition(ontology, "Luxembourg", luxembourg_type, NULL);
    // CaymanIslands
    kos_term* caymanislands_type = kos_mk_prop("CaymanIslands");
    kos_ontology_add_type_definition(ontology, "CaymanIslands", caymanislands_type, NULL);
    // BritishVirginIslands
    kos_term* britishvirginislands_type = kos_mk_prop("BritishVirginIslands");
    kos_ontology_add_type_definition(ontology, "BritishVirginIslands", britishvirginislands_type, NULL);
    // Bermuda
    kos_term* bermuda_type = kos_mk_prop("Bermuda");
    kos_ontology_add_type_definition(ontology, "Bermuda", bermuda_type, NULL);
    // Panama
    kos_term* panama_type = kos_mk_prop("Panama");
    kos_ontology_add_type_definition(ontology, "Panama", panama_type, NULL);
    // Bahamas
    kos_term* bahamas_type = kos_mk_prop("Bahamas");
    kos_ontology_add_type_definition(ontology, "Bahamas", bahamas_type, NULL);
    // Jersey
    kos_term* jersey_type = kos_mk_prop("Jersey");
    kos_ontology_add_type_definition(ontology, "Jersey", jersey_type, NULL);
    // Guernsey
    kos_term* guernsey_type = kos_mk_prop("Guernsey");
    kos_ontology_add_type_definition(ontology, "Guernsey", guernsey_type, NULL);
    // IsleOfMan
    kos_term* isleofman_type = kos_mk_prop("IsleOfMan");
    kos_ontology_add_type_definition(ontology, "IsleOfMan", isleofman_type, NULL);
    // Cyprus
    kos_term* cyprus_type = kos_mk_prop("Cyprus");
    kos_ontology_add_type_definition(ontology, "Cyprus", cyprus_type, NULL);
    // Malta
    kos_term* malta_type = kos_mk_prop("Malta");
    kos_ontology_add_type_definition(ontology, "Malta", malta_type, NULL);
    // Ireland
    kos_term* ireland_type = kos_mk_prop("Ireland");
    kos_ontology_add_type_definition(ontology, "Ireland", ireland_type, NULL);
    // Netherlands
    kos_term* netherlands_type = kos_mk_prop("Netherlands");
    kos_ontology_add_type_definition(ontology, "Netherlands", netherlands_type, NULL);
    // Belgium
    kos_term* belgium_type = kos_mk_prop("Belgium");
    kos_ontology_add_type_definition(ontology, "Belgium", belgium_type, NULL);
    // Germany
    kos_term* germany_type = kos_mk_prop("Germany");
    kos_ontology_add_type_definition(ontology, "Germany", germany_type, NULL);
    // France
    kos_term* france_type = kos_mk_prop("France");
    kos_ontology_add_type_definition(ontology, "France", france_type, NULL);
    // Italy
    kos_term* italy_type = kos_mk_prop("Italy");
    kos_ontology_add_type_definition(ontology, "Italy", italy_type, NULL);
    // Spain
    kos_term* spain_type = kos_mk_prop("Spain");
    kos_ontology_add_type_definition(ontology, "Spain", spain_type, NULL);
    // Portugal
    kos_term* portugal_type = kos_mk_prop("Portugal");
    kos_ontology_add_type_definition(ontology, "Portugal", portugal_type, NULL);
    // Greece
    kos_term* greece_type = kos_mk_prop("Greece");
    kos_ontology_add_type_definition(ontology, "Greece", greece_type, NULL);
    // Austria
    kos_term* austria_type = kos_mk_prop("Austria");
    kos_ontology_add_type_definition(ontology, "Austria", austria_type, NULL);
    // Sweden
    kos_term* sweden_type = kos_mk_prop("Sweden");
    kos_ontology_add_type_definition(ontology, "Sweden", sweden_type, NULL);
    // Norway
    kos_term* norway_type = kos_mk_prop("Norway");
    kos_ontology_add_type_definition(ontology, "Norway", norway_type, NULL);
    // Denmark
    kos_term* denmark_type = kos_mk_prop("Denmark");
    kos_ontology_add_type_definition(ontology, "Denmark", denmark_type, NULL);
    // Finland
    kos_term* finland_type = kos_mk_prop("Finland");
    kos_ontology_add_type_definition(ontology, "Finland", finland_type, NULL);
    // Iceland
    kos_term* iceland_type = kos_mk_prop("Iceland");
    kos_ontology_add_type_definition(ontology, "Iceland", iceland_type, NULL);
    // Poland
    kos_term* poland_type = kos_mk_prop("Poland");
    kos_ontology_add_type_definition(ontology, "Poland", poland_type, NULL);
    // CzechRepublic
    kos_term* czechrepublic_type = kos_mk_prop("CzechRepublic");
    kos_ontology_add_type_definition(ontology, "CzechRepublic", czechrepublic_type, NULL);
    // Hungary
    kos_term* hungary_type = kos_mk_prop("Hungary");
    kos_ontology_add_type_definition(ontology, "Hungary", hungary_type, NULL);
    // Romania
    kos_term* romania_type = kos_mk_prop("Romania");
    kos_ontology_add_type_definition(ontology, "Romania", romania_type, NULL);
    // Bulgaria
    kos_term* bulgaria_type = kos_mk_prop("Bulgaria");
    kos_ontology_add_type_definition(ontology, "Bulgaria", bulgaria_type, NULL);
    // Croatia
    kos_term* croatia_type = kos_mk_prop("Croatia");
    kos_ontology_add_type_definition(ontology, "Croatia", croatia_type, NULL);
    // Japan
    kos_term* japan_type = kos_mk_prop("Japan");
    kos_ontology_add_type_definition(ontology, "Japan", japan_type, NULL);
    // China
    kos_term* china_type = kos_mk_prop("China");
    kos_ontology_add_type_definition(ontology, "China", china_type, NULL);
    // SouthKorea
    kos_term* southkorea_type = kos_mk_prop("SouthKorea");
    kos_ontology_add_type_definition(ontology, "SouthKorea", southkorea_type, NULL);
    // India
    kos_term* india_type = kos_mk_prop("India");
    kos_ontology_add_type_definition(ontology, "India", india_type, NULL);
    // Australia
    kos_term* australia_type = kos_mk_prop("Australia");
    kos_ontology_add_type_definition(ontology, "Australia", australia_type, NULL);
    // NewZealand
    kos_term* newzealand_type = kos_mk_prop("NewZealand");
    kos_ontology_add_type_definition(ontology, "NewZealand", newzealand_type, NULL);
    // Canada
    kos_term* canada_type = kos_mk_prop("Canada");
    kos_ontology_add_type_definition(ontology, "Canada", canada_type, NULL);
    // Mexico
    kos_term* mexico_type = kos_mk_prop("Mexico");
    kos_ontology_add_type_definition(ontology, "Mexico", mexico_type, NULL);
    // Brazil
    kos_term* brazil_type = kos_mk_prop("Brazil");
    kos_ontology_add_type_definition(ontology, "Brazil", brazil_type, NULL);
    // Argentina
    kos_term* argentina_type = kos_mk_prop("Argentina");
    kos_ontology_add_type_definition(ontology, "Argentina", argentina_type, NULL);
    // Chile
    kos_term* chile_type = kos_mk_prop("Chile");
    kos_ontology_add_type_definition(ontology, "Chile", chile_type, NULL);
    // Colombia
    kos_term* colombia_type = kos_mk_prop("Colombia");
    kos_ontology_add_type_definition(ontology, "Colombia", colombia_type, NULL);
    // SouthAfrica
    kos_term* southafrica_type = kos_mk_prop("SouthAfrica");
    kos_ontology_add_type_definition(ontology, "SouthAfrica", southafrica_type, NULL);
    // Egypt
    kos_term* egypt_type = kos_mk_prop("Egypt");
    kos_ontology_add_type_definition(ontology, "Egypt", egypt_type, NULL);
    // Nigeria
    kos_term* nigeria_type = kos_mk_prop("Nigeria");
    kos_ontology_add_type_definition(ontology, "Nigeria", nigeria_type, NULL);
    // Kenya
    kos_term* kenya_type = kos_mk_prop("Kenya");
    kos_ontology_add_type_definition(ontology, "Kenya", kenya_type, NULL);
    // UAE
    kos_term* uae_type = kos_mk_prop("UAE");
    kos_ontology_add_type_definition(ontology, "UAE", uae_type, NULL);
    // SaudiArabia
    kos_term* saudiarabia_type = kos_mk_prop("SaudiArabia");
    kos_ontology_add_type_definition(ontology, "SaudiArabia", saudiarabia_type, NULL);
    // Israel
    kos_term* israel_type = kos_mk_prop("Israel");
    kos_ontology_add_type_definition(ontology, "Israel", israel_type, NULL);
    // Turkey
    kos_term* turkey_type = kos_mk_prop("Turkey");
    kos_ontology_add_type_definition(ontology, "Turkey", turkey_type, NULL);
    // Russia
    kos_term* russia_type = kos_mk_prop("Russia");
    kos_ontology_add_type_definition(ontology, "Russia", russia_type, NULL);
    // Ukraine
    kos_term* ukraine_type = kos_mk_prop("Ukraine");
    kos_ontology_add_type_definition(ontology, "Ukraine", ukraine_type, NULL);
    // Kazakhstan
    kos_term* kazakhstan_type = kos_mk_prop("Kazakhstan");
    kos_ontology_add_type_definition(ontology, "Kazakhstan", kazakhstan_type, NULL);
    // Indonesia
    kos_term* indonesia_type = kos_mk_prop("Indonesia");
    kos_ontology_add_type_definition(ontology, "Indonesia", indonesia_type, NULL);
    // Malaysia
    kos_term* malaysia_type = kos_mk_prop("Malaysia");
    kos_ontology_add_type_definition(ontology, "Malaysia", malaysia_type, NULL);
    // Thailand
    kos_term* thailand_type = kos_mk_prop("Thailand");
    kos_ontology_add_type_definition(ontology, "Thailand", thailand_type, NULL);
    // Vietnam
    kos_term* vietnam_type = kos_mk_prop("Vietnam");
    kos_ontology_add_type_definition(ontology, "Vietnam", vietnam_type, NULL);
    // Philippines
    kos_term* philippines_type = kos_mk_prop("Philippines");
    kos_ontology_add_type_definition(ontology, "Philippines", philippines_type, NULL);
    // Bangladesh
    kos_term* bangladesh_type = kos_mk_prop("Bangladesh");
    kos_ontology_add_type_definition(ontology, "Bangladesh", bangladesh_type, NULL);
    // Pakistan
    kos_term* pakistan_type = kos_mk_prop("Pakistan");
    kos_ontology_add_type_definition(ontology, "Pakistan", pakistan_type, NULL);
    // SriLanka
    kos_term* srilanka_type = kos_mk_prop("SriLanka");
    kos_ontology_add_type_definition(ontology, "SriLanka", srilanka_type, NULL);
    // Myanmar
    kos_term* myanmar_type = kos_mk_prop("Myanmar");
    kos_ontology_add_type_definition(ontology, "Myanmar", myanmar_type, NULL);
    // Cambodia
    kos_term* cambodia_type = kos_mk_prop("Cambodia");
    kos_ontology_add_type_definition(ontology, "Cambodia", cambodia_type, NULL);
    // Laos
    kos_term* laos_type = kos_mk_prop("Laos");
    kos_ontology_add_type_definition(ontology, "Laos", laos_type, NULL);
    // Mongolia
    kos_term* mongolia_type = kos_mk_prop("Mongolia");
    kos_ontology_add_type_definition(ontology, "Mongolia", mongolia_type, NULL);
    // Nepal
    kos_term* nepal_type = kos_mk_prop("Nepal");
    kos_ontology_add_type_definition(ontology, "Nepal", nepal_type, NULL);
    // Bhutan
    kos_term* bhutan_type = kos_mk_prop("Bhutan");
    kos_ontology_add_type_definition(ontology, "Bhutan", bhutan_type, NULL);
    // Afghanistan
    kos_term* afghanistan_type = kos_mk_prop("Afghanistan");
    kos_ontology_add_type_definition(ontology, "Afghanistan", afghanistan_type, NULL);
    // Iran
    kos_term* iran_type = kos_mk_prop("Iran");
    kos_ontology_add_type_definition(ontology, "Iran", iran_type, NULL);
    // Iraq
    kos_term* iraq_type = kos_mk_prop("Iraq");
    kos_ontology_add_type_definition(ontology, "Iraq", iraq_type, NULL);
    // Jordan
    kos_term* jordan_type = kos_mk_prop("Jordan");
    kos_ontology_add_type_definition(ontology, "Jordan", jordan_type, NULL);
    // Lebanon
    kos_term* lebanon_type = kos_mk_prop("Lebanon");
    kos_ontology_add_type_definition(ontology, "Lebanon", lebanon_type, NULL);
    // Syria
    kos_term* syria_type = kos_mk_prop("Syria");
    kos_ontology_add_type_definition(ontology, "Syria", syria_type, NULL);
    // Yemen
    kos_term* yemen_type = kos_mk_prop("Yemen");
    kos_ontology_add_type_definition(ontology, "Yemen", yemen_type, NULL);
    // Oman
    kos_term* oman_type = kos_mk_prop("Oman");
    kos_ontology_add_type_definition(ontology, "Oman", oman_type, NULL);
    // Qatar
    kos_term* qatar_type = kos_mk_prop("Qatar");
    kos_ontology_add_type_definition(ontology, "Qatar", qatar_type, NULL);
    // Kuwait
    kos_term* kuwait_type = kos_mk_prop("Kuwait");
    kos_ontology_add_type_definition(ontology, "Kuwait", kuwait_type, NULL);
    // Bahrain
    kos_term* bahrain_type = kos_mk_prop("Bahrain");
    kos_ontology_add_type_definition(ontology, "Bahrain", bahrain_type, NULL);
    // Morocco
    kos_term* morocco_type = kos_mk_prop("Morocco");
    kos_ontology_add_type_definition(ontology, "Morocco", morocco_type, NULL);
    // Algeria
    kos_term* algeria_type = kos_mk_prop("Algeria");
    kos_ontology_add_type_definition(ontology, "Algeria", algeria_type, NULL);
    // Tunisia
    kos_term* tunisia_type = kos_mk_prop("Tunisia");
    kos_ontology_add_type_definition(ontology, "Tunisia", tunisia_type, NULL);
    // Libya
    kos_term* libya_type = kos_mk_prop("Libya");
    kos_ontology_add_type_definition(ontology, "Libya", libya_type, NULL);
    // Sudan
    kos_term* sudan_type = kos_mk_prop("Sudan");
    kos_ontology_add_type_definition(ontology, "Sudan", sudan_type, NULL);
    // Ethiopia
    kos_term* ethiopia_type = kos_mk_prop("Ethiopia");
    kos_ontology_add_type_definition(ontology, "Ethiopia", ethiopia_type, NULL);
    // Tanzania
    kos_term* tanzania_type = kos_mk_prop("Tanzania");
    kos_ontology_add_type_definition(ontology, "Tanzania", tanzania_type, NULL);
    // Uganda
    kos_term* uganda_type = kos_mk_prop("Uganda");
    kos_ontology_add_type_definition(ontology, "Uganda", uganda_type, NULL);
    // Ghana
    kos_term* ghana_type = kos_mk_prop("Ghana");
    kos_ontology_add_type_definition(ontology, "Ghana", ghana_type, NULL);
    // IvoryCoast
    kos_term* ivorycoast_type = kos_mk_prop("IvoryCoast");
    kos_ontology_add_type_definition(ontology, "IvoryCoast", ivorycoast_type, NULL);
    // Senegal
    kos_term* senegal_type = kos_mk_prop("Senegal");
    kos_ontology_add_type_definition(ontology, "Senegal", senegal_type, NULL);
    // Angola
    kos_term* angola_type = kos_mk_prop("Angola");
    kos_ontology_add_type_definition(ontology, "Angola", angola_type, NULL);
    // Mozambique
    kos_term* mozambique_type = kos_mk_prop("Mozambique");
    kos_ontology_add_type_definition(ontology, "Mozambique", mozambique_type, NULL);
    // Zimbabwe
    kos_term* zimbabwe_type = kos_mk_prop("Zimbabwe");
    kos_ontology_add_type_definition(ontology, "Zimbabwe", zimbabwe_type, NULL);
    // Botswana
    kos_term* botswana_type = kos_mk_prop("Botswana");
    kos_ontology_add_type_definition(ontology, "Botswana", botswana_type, NULL);
    // Namibia
    kos_term* namibia_type = kos_mk_prop("Namibia");
    kos_ontology_add_type_definition(ontology, "Namibia", namibia_type, NULL);
    // Mauritius
    kos_term* mauritius_type = kos_mk_prop("Mauritius");
    kos_ontology_add_type_definition(ontology, "Mauritius", mauritius_type, NULL);
    // Seychelles
    kos_term* seychelles_type = kos_mk_prop("Seychelles");
    kos_ontology_add_type_definition(ontology, "Seychelles", seychelles_type, NULL);
    // Maldives
    kos_term* maldives_type = kos_mk_prop("Maldives");
    kos_ontology_add_type_definition(ontology, "Maldives", maldives_type, NULL);
    // Fiji
    kos_term* fiji_type = kos_mk_prop("Fiji");
    kos_ontology_add_type_definition(ontology, "Fiji", fiji_type, NULL);
    // ========== currencies ==========\n    // USD
    kos_term* usd_type = kos_mk_prop("USD");
    kos_ontology_add_type_definition(ontology, "USD", usd_type, NULL);
    // EUR
    kos_term* eur_type = kos_mk_prop("EUR");
    kos_ontology_add_type_definition(ontology, "EUR", eur_type, NULL);
    // GBP
    kos_term* gbp_type = kos_mk_prop("GBP");
    kos_ontology_add_type_definition(ontology, "GBP", gbp_type, NULL);
    // JPY
    kos_term* jpy_type = kos_mk_prop("JPY");
    kos_ontology_add_type_definition(ontology, "JPY", jpy_type, NULL);
    // CHF
    kos_term* chf_type = kos_mk_prop("CHF");
    kos_ontology_add_type_definition(ontology, "CHF", chf_type, NULL);
    // CNY
    kos_term* cny_type = kos_mk_prop("CNY");
    kos_ontology_add_type_definition(ontology, "CNY", cny_type, NULL);
    // HKD
    kos_term* hkd_type = kos_mk_prop("HKD");
    kos_ontology_add_type_definition(ontology, "HKD", hkd_type, NULL);
    // SGD
    kos_term* sgd_type = kos_mk_prop("SGD");
    kos_ontology_add_type_definition(ontology, "SGD", sgd_type, NULL);
    // AUD
    kos_term* aud_type = kos_mk_prop("AUD");
    kos_ontology_add_type_definition(ontology, "AUD", aud_type, NULL);
    // CAD
    kos_term* cad_type = kos_mk_prop("CAD");
    kos_ontology_add_type_definition(ontology, "CAD", cad_type, NULL);
    // NZD
    kos_term* nzd_type = kos_mk_prop("NZD");
    kos_ontology_add_type_definition(ontology, "NZD", nzd_type, NULL);
    // SEK
    kos_term* sek_type = kos_mk_prop("SEK");
    kos_ontology_add_type_definition(ontology, "SEK", sek_type, NULL);
    // NOK
    kos_term* nok_type = kos_mk_prop("NOK");
    kos_ontology_add_type_definition(ontology, "NOK", nok_type, NULL);
    // DKK
    kos_term* dkk_type = kos_mk_prop("DKK");
    kos_ontology_add_type_definition(ontology, "DKK", dkk_type, NULL);
    // PLN
    kos_term* pln_type = kos_mk_prop("PLN");
    kos_ontology_add_type_definition(ontology, "PLN", pln_type, NULL);
    // CZK
    kos_term* czk_type = kos_mk_prop("CZK");
    kos_ontology_add_type_definition(ontology, "CZK", czk_type, NULL);
    // HUF
    kos_term* huf_type = kos_mk_prop("HUF");
    kos_ontology_add_type_definition(ontology, "HUF", huf_type, NULL);
    // RUB
    kos_term* rub_type = kos_mk_prop("RUB");
    kos_ontology_add_type_definition(ontology, "RUB", rub_type, NULL);
    // INR
    kos_term* inr_type = kos_mk_prop("INR");
    kos_ontology_add_type_definition(ontology, "INR", inr_type, NULL);
    // KRW
    kos_term* krw_type = kos_mk_prop("KRW");
    kos_ontology_add_type_definition(ontology, "KRW", krw_type, NULL);
    // BRL
    kos_term* brl_type = kos_mk_prop("BRL");
    kos_ontology_add_type_definition(ontology, "BRL", brl_type, NULL);
    // MXN
    kos_term* mxn_type = kos_mk_prop("MXN");
    kos_ontology_add_type_definition(ontology, "MXN", mxn_type, NULL);
    // ZAR
    kos_term* zar_type = kos_mk_prop("ZAR");
    kos_ontology_add_type_definition(ontology, "ZAR", zar_type, NULL);
    // TRY
    kos_term* try_type = kos_mk_prop("TRY");
    kos_ontology_add_type_definition(ontology, "TRY", try_type, NULL);
    // AED
    kos_term* aed_type = kos_mk_prop("AED");
    kos_ontology_add_type_definition(ontology, "AED", aed_type, NULL);
    // SAR
    kos_term* sar_type = kos_mk_prop("SAR");
    kos_ontology_add_type_definition(ontology, "SAR", sar_type, NULL);
    // ILS
    kos_term* ils_type = kos_mk_prop("ILS");
    kos_ontology_add_type_definition(ontology, "ILS", ils_type, NULL);
    // THB
    kos_term* thb_type = kos_mk_prop("THB");
    kos_ontology_add_type_definition(ontology, "THB", thb_type, NULL);
    // MYR
    kos_term* myr_type = kos_mk_prop("MYR");
    kos_ontology_add_type_definition(ontology, "MYR", myr_type, NULL);
    // IDR
    kos_term* idr_type = kos_mk_prop("IDR");
    kos_ontology_add_type_definition(ontology, "IDR", idr_type, NULL);
    // PHP
    kos_term* php_type = kos_mk_prop("PHP");
    kos_ontology_add_type_definition(ontology, "PHP", php_type, NULL);
    // VND
    kos_term* vnd_type = kos_mk_prop("VND");
    kos_ontology_add_type_definition(ontology, "VND", vnd_type, NULL);
    // BTC
    kos_term* btc_type = kos_mk_prop("BTC");
    kos_ontology_add_type_definition(ontology, "BTC", btc_type, NULL);
    // ETH
    kos_term* eth_type = kos_mk_prop("ETH");
    kos_ontology_add_type_definition(ontology, "ETH", eth_type, NULL);
    // USDT
    kos_term* usdt_type = kos_mk_prop("USDT");
    kos_ontology_add_type_definition(ontology, "USDT", usdt_type, NULL);
    // USDC
    kos_term* usdc_type = kos_mk_prop("USDC");
    kos_ontology_add_type_definition(ontology, "USDC", usdc_type, NULL);
    // BNB
    kos_term* bnb_type = kos_mk_prop("BNB");
    kos_ontology_add_type_definition(ontology, "BNB", bnb_type, NULL);
    // XRP
    kos_term* xrp_type = kos_mk_prop("XRP");
    kos_ontology_add_type_definition(ontology, "XRP", xrp_type, NULL);
    // ADA
    kos_term* ada_type = kos_mk_prop("ADA");
    kos_ontology_add_type_definition(ontology, "ADA", ada_type, NULL);
    // SOL
    kos_term* sol_type = kos_mk_prop("SOL");
    kos_ontology_add_type_definition(ontology, "SOL", sol_type, NULL);
    // DOGE
    kos_term* doge_type = kos_mk_prop("DOGE");
    kos_ontology_add_type_definition(ontology, "DOGE", doge_type, NULL);
    // DOT
    kos_term* dot_type = kos_mk_prop("DOT");
    kos_ontology_add_type_definition(ontology, "DOT", dot_type, NULL);
    // MATIC
    kos_term* matic_type = kos_mk_prop("MATIC");
    kos_ontology_add_type_definition(ontology, "MATIC", matic_type, NULL);
    // AVAX
    kos_term* avax_type = kos_mk_prop("AVAX");
    kos_ontology_add_type_definition(ontology, "AVAX", avax_type, NULL);
    // LINK
    kos_term* link_type = kos_mk_prop("LINK");
    kos_ontology_add_type_definition(ontology, "LINK", link_type, NULL);
    // UNI
    kos_term* uni_type = kos_mk_prop("UNI");
    kos_ontology_add_type_definition(ontology, "UNI", uni_type, NULL);
    // LTC
    kos_term* ltc_type = kos_mk_prop("LTC");
    kos_ontology_add_type_definition(ontology, "LTC", ltc_type, NULL);
    // BCH
    kos_term* bch_type = kos_mk_prop("BCH");
    kos_ontology_add_type_definition(ontology, "BCH", bch_type, NULL);
    // XLM
    kos_term* xlm_type = kos_mk_prop("XLM");
    kos_ontology_add_type_definition(ontology, "XLM", xlm_type, NULL);
    // ATOM
    kos_term* atom_type = kos_mk_prop("ATOM");
    kos_ontology_add_type_definition(ontology, "ATOM", atom_type, NULL);
    // ALGO
    kos_term* algo_type = kos_mk_prop("ALGO");
    kos_ontology_add_type_definition(ontology, "ALGO", algo_type, NULL);
    // VET
    kos_term* vet_type = kos_mk_prop("VET");
    kos_ontology_add_type_definition(ontology, "VET", vet_type, NULL);
    // ICP
    kos_term* icp_type = kos_mk_prop("ICP");
    kos_ontology_add_type_definition(ontology, "ICP", icp_type, NULL);
    // FIL
    kos_term* fil_type = kos_mk_prop("FIL");
    kos_ontology_add_type_definition(ontology, "FIL", fil_type, NULL);
    // TRX
    kos_term* trx_type = kos_mk_prop("TRX");
    kos_ontology_add_type_definition(ontology, "TRX", trx_type, NULL);
    // ETC
    kos_term* etc_type = kos_mk_prop("ETC");
    kos_ontology_add_type_definition(ontology, "ETC", etc_type, NULL);
    // ========== transaction_statuses ==========\n    // Pending
    kos_term* pending_type = kos_mk_prop("Pending");
    kos_ontology_add_type_definition(ontology, "Pending", pending_type, NULL);
    // Processing
    kos_term* processing_type = kos_mk_prop("Processing");
    kos_ontology_add_type_definition(ontology, "Processing", processing_type, NULL);
    // Completed
    kos_term* completed_type = kos_mk_prop("Completed");
    kos_ontology_add_type_definition(ontology, "Completed", completed_type, NULL);
    // Failed
    kos_term* failed_type = kos_mk_prop("Failed");
    kos_ontology_add_type_definition(ontology, "Failed", failed_type, NULL);
    // Cancelled
    kos_term* cancelled_type = kos_mk_prop("Cancelled");
    kos_ontology_add_type_definition(ontology, "Cancelled", cancelled_type, NULL);
    // Reversed
    kos_term* reversed_type = kos_mk_prop("Reversed");
    kos_ontology_add_type_definition(ontology, "Reversed", reversed_type, NULL);
    // Refunded
    kos_term* refunded_type = kos_mk_prop("Refunded");
    kos_ontology_add_type_definition(ontology, "Refunded", refunded_type, NULL);
    // Suspended
    kos_term* suspended_type = kos_mk_prop("Suspended");
    kos_ontology_add_type_definition(ontology, "Suspended", suspended_type, NULL);
    // Frozen
    kos_term* frozen_type = kos_mk_prop("Frozen");
    kos_ontology_add_type_definition(ontology, "Frozen", frozen_type, NULL);
    // Blocked
    kos_term* blocked_type = kos_mk_prop("Blocked");
    kos_ontology_add_type_definition(ontology, "Blocked", blocked_type, NULL);
    // UnderReview
    kos_term* underreview_type = kos_mk_prop("UnderReview");
    kos_ontology_add_type_definition(ontology, "UnderReview", underreview_type, NULL);
    // RequiresVerification
    kos_term* requiresverification_type = kos_mk_prop("RequiresVerification");
    kos_ontology_add_type_definition(ontology, "RequiresVerification", requiresverification_type, NULL);
    // Flagged
    kos_term* flagged_type = kos_mk_prop("Flagged");
    kos_ontology_add_type_definition(ontology, "Flagged", flagged_type, NULL);
    // Rejected
    kos_term* rejected_type = kos_mk_prop("Rejected");
    kos_ontology_add_type_definition(ontology, "Rejected", rejected_type, NULL);
    // Approved
    kos_term* approved_type = kos_mk_prop("Approved");
    kos_ontology_add_type_definition(ontology, "Approved", approved_type, NULL);
    // Declined
    kos_term* declined_type = kos_mk_prop("Declined");
    kos_ontology_add_type_definition(ontology, "Declined", declined_type, NULL);
    // Timeout
    kos_term* timeout_type = kos_mk_prop("Timeout");
    kos_ontology_add_type_definition(ontology, "Timeout", timeout_type, NULL);
    // Expired
    kos_term* expired_type = kos_mk_prop("Expired");
    kos_ontology_add_type_definition(ontology, "Expired", expired_type, NULL);
    // Invalid
    kos_term* invalid_type = kos_mk_prop("Invalid");
    kos_ontology_add_type_definition(ontology, "Invalid", invalid_type, NULL);
    // Duplicate
    kos_term* duplicate_type = kos_mk_prop("Duplicate");
    kos_ontology_add_type_definition(ontology, "Duplicate", duplicate_type, NULL);
    // Fraudulent
    kos_term* fraudulent_type = kos_mk_prop("Fraudulent");
    kos_ontology_add_type_definition(ontology, "Fraudulent", fraudulent_type, NULL);
    // Chargeback
    kos_term* chargeback_type = kos_mk_prop("Chargeback");
    kos_ontology_add_type_definition(ontology, "Chargeback", chargeback_type, NULL);
    // Disputed
    kos_term* disputed_type = kos_mk_prop("Disputed");
    kos_ontology_add_type_definition(ontology, "Disputed", disputed_type, NULL);
    // Resolved
    kos_term* resolved_type = kos_mk_prop("Resolved");
    kos_ontology_add_type_definition(ontology, "Resolved", resolved_type, NULL);
    // ========== risk_levels ==========\n    // LowRisk
    kos_term* lowrisk_type = kos_mk_prop("LowRisk");
    kos_ontology_add_type_definition(ontology, "LowRisk", lowrisk_type, NULL);
    // MediumRisk
    kos_term* mediumrisk_type = kos_mk_prop("MediumRisk");
    kos_ontology_add_type_definition(ontology, "MediumRisk", mediumrisk_type, NULL);
    // HighRisk
    kos_term* highrisk_type = kos_mk_prop("HighRisk");
    kos_ontology_add_type_definition(ontology, "HighRisk", highrisk_type, NULL);
    // VeryHighRisk
    kos_term* veryhighrisk_type = kos_mk_prop("VeryHighRisk");
    kos_ontology_add_type_definition(ontology, "VeryHighRisk", veryhighrisk_type, NULL);
    // CriticalRisk
    kos_term* criticalrisk_type = kos_mk_prop("CriticalRisk");
    kos_ontology_add_type_definition(ontology, "CriticalRisk", criticalrisk_type, NULL);
    // Sanctioned
    kos_term* sanctioned_type = kos_mk_prop("Sanctioned");
    kos_ontology_add_type_definition(ontology, "Sanctioned", sanctioned_type, NULL);
    // PEP
    kos_term* pep_type = kos_mk_prop("PEP");
    kos_ontology_add_type_definition(ontology, "PEP", pep_type, NULL);
    // AdverseMedia
    kos_term* adversemedia_type = kos_mk_prop("AdverseMedia");
    kos_ontology_add_type_definition(ontology, "AdverseMedia", adversemedia_type, NULL);
    // SuspiciousActivity
    kos_term* suspiciousactivity_type = kos_mk_prop("SuspiciousActivity");
    kos_ontology_add_type_definition(ontology, "SuspiciousActivity", suspiciousactivity_type, NULL);
    // TerroristFinancing
    kos_term* terroristfinancing_type = kos_mk_prop("TerroristFinancing");
    kos_ontology_add_type_definition(ontology, "TerroristFinancing", terroristfinancing_type, NULL);
    // DrugTrafficking
    kos_term* drugtrafficking_type = kos_mk_prop("DrugTrafficking");
    kos_ontology_add_type_definition(ontology, "DrugTrafficking", drugtrafficking_type, NULL);
    // HumanTrafficking
    kos_term* humantrafficking_type = kos_mk_prop("HumanTrafficking");
    kos_ontology_add_type_definition(ontology, "HumanTrafficking", humantrafficking_type, NULL);
    // Corruption
    kos_term* corruption_type = kos_mk_prop("Corruption");
    kos_ontology_add_type_definition(ontology, "Corruption", corruption_type, NULL);
    // TaxEvasion
    kos_term* taxevasion_type = kos_mk_prop("TaxEvasion");
    kos_ontology_add_type_definition(ontology, "TaxEvasion", taxevasion_type, NULL);
    // MarketManipulation
    kos_term* marketmanipulation_type = kos_mk_prop("MarketManipulation");
    kos_ontology_add_type_definition(ontology, "MarketManipulation", marketmanipulation_type, NULL);
    // InsiderTrading
    kos_term* insidertrading_type = kos_mk_prop("InsiderTrading");
    kos_ontology_add_type_definition(ontology, "InsiderTrading", insidertrading_type, NULL);
    // ========== compliance_types ==========\n    // KYC
    kos_term* kyc_type = kos_mk_prop("KYC");
    kos_ontology_add_type_definition(ontology, "KYC", kyc_type, NULL);
    // AML
    kos_term* aml_type = kos_mk_prop("AML");
    kos_ontology_add_type_definition(ontology, "AML", aml_type, NULL);
    // CTF
    kos_term* ctf_type = kos_mk_prop("CTF");
    kos_ontology_add_type_definition(ontology, "CTF", ctf_type, NULL);
    // SanctionsScreening
    kos_term* sanctionsscreening_type = kos_mk_prop("SanctionsScreening");
    kos_ontology_add_type_definition(ontology, "SanctionsScreening", sanctionsscreening_type, NULL);
    // PEPCheck
    kos_term* pepcheck_type = kos_mk_prop("PEPCheck");
    kos_ontology_add_type_definition(ontology, "PEPCheck", pepcheck_type, NULL);
    // AdverseMediaCheck
    kos_term* adversemediacheck_type = kos_mk_prop("AdverseMediaCheck");
    kos_ontology_add_type_definition(ontology, "AdverseMediaCheck", adversemediacheck_type, NULL);
    // TransactionMonitoring
    kos_term* transactionmonitoring_type = kos_mk_prop("TransactionMonitoring");
    kos_ontology_add_type_definition(ontology, "TransactionMonitoring", transactionmonitoring_type, NULL);
    // SuspiciousActivityReport
    kos_term* suspiciousactivityreport_type = kos_mk_prop("SuspiciousActivityReport");
    kos_ontology_add_type_definition(ontology, "SuspiciousActivityReport", suspiciousactivityreport_type, NULL);
    // CurrencyTransactionReport
    kos_term* currencytransactionreport_type = kos_mk_prop("CurrencyTransactionReport");
    kos_ontology_add_type_definition(ontology, "CurrencyTransactionReport", currencytransactionreport_type, NULL);
    // ForeignAccountTaxCompliance
    kos_term* foreignaccounttaxcompliance_type = kos_mk_prop("ForeignAccountTaxCompliance");
    kos_ontology_add_type_definition(ontology, "ForeignAccountTaxCompliance", foreignaccounttaxcompliance_type, NULL);
    // CRS
    kos_term* crs_type = kos_mk_prop("CRS");
    kos_ontology_add_type_definition(ontology, "CRS", crs_type, NULL);
    // FATCA
    kos_term* fatca_type = kos_mk_prop("FATCA");
    kos_ontology_add_type_definition(ontology, "FATCA", fatca_type, NULL);
    // GDPR
    kos_term* gdpr_type = kos_mk_prop("GDPR");
    kos_ontology_add_type_definition(ontology, "GDPR", gdpr_type, NULL);
    // PCI
    kos_term* pci_type = kos_mk_prop("PCI");
    kos_ontology_add_type_definition(ontology, "PCI", pci_type, NULL);
    // SOX
    kos_term* sox_type = kos_mk_prop("SOX");
    kos_ontology_add_type_definition(ontology, "SOX", sox_type, NULL);
    // MiFID
    kos_term* mifid_type = kos_mk_prop("MiFID");
    kos_ontology_add_type_definition(ontology, "MiFID", mifid_type, NULL);
    // EMIR
    kos_term* emir_type = kos_mk_prop("EMIR");
    kos_ontology_add_type_definition(ontology, "EMIR", emir_type, NULL);
    // DoddFrank
    kos_term* doddfrank_type = kos_mk_prop("DoddFrank");
    kos_ontology_add_type_definition(ontology, "DoddFrank", doddfrank_type, NULL);
    // BaselIII
    kos_term* baseliii_type = kos_mk_prop("BaselIII");
    kos_ontology_add_type_definition(ontology, "BaselIII", baseliii_type, NULL);
    // SolvencyII
    kos_term* solvencyii_type = kos_mk_prop("SolvencyII");
    kos_ontology_add_type_definition(ontology, "SolvencyII", solvencyii_type, NULL);
    // PSD2
    kos_term* psd2_type = kos_mk_prop("PSD2");
    kos_ontology_add_type_definition(ontology, "PSD2", psd2_type, NULL);
    // GDPR (already added, skipping duplicate)\n    // CCPA
    kos_term* ccpa_type = kos_mk_prop("CCPA");
    kos_ontology_add_type_definition(ontology, "CCPA", ccpa_type, NULL);
    // ========== money_laundering_methods ==========\n    // Structuring
    kos_term* structuring_type = kos_mk_prop("Structuring");
    kos_ontology_add_type_definition(ontology, "Structuring", structuring_type, NULL);
    // Smurfing
    kos_term* smurfing_type = kos_mk_prop("Smurfing");
    kos_ontology_add_type_definition(ontology, "Smurfing", smurfing_type, NULL);
    // Layering
    kos_term* layering_type = kos_mk_prop("Layering");
    kos_ontology_add_type_definition(ontology, "Layering", layering_type, NULL);
    // Integration
    kos_term* integration_type = kos_mk_prop("Integration");
    kos_ontology_add_type_definition(ontology, "Integration", integration_type, NULL);
    // TradeBased
    kos_term* tradebased_type = kos_mk_prop("TradeBased");
    kos_ontology_add_type_definition(ontology, "TradeBased", tradebased_type, NULL);
    // ShellCompany
    kos_term* shellcompany_type = kos_mk_prop("ShellCompany");
    kos_ontology_add_type_definition(ontology, "ShellCompany", shellcompany_type, NULL);
    // NomineeAccount (already added, skipping duplicate)\n    // OffshoreStructure
    kos_term* offshorestructure_type = kos_mk_prop("OffshoreStructure");
    kos_ontology_add_type_definition(ontology, "OffshoreStructure", offshorestructure_type, NULL);
    // CryptocurrencyMixer
    kos_term* cryptocurrencymixer_type = kos_mk_prop("CryptocurrencyMixer");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyMixer", cryptocurrencymixer_type, NULL);
    // Hawala
    kos_term* hawala_type = kos_mk_prop("Hawala");
    kos_ontology_add_type_definition(ontology, "Hawala", hawala_type, NULL);
    // UndergroundBanking
    kos_term* undergroundbanking_type = kos_mk_prop("UndergroundBanking");
    kos_ontology_add_type_definition(ontology, "UndergroundBanking", undergroundbanking_type, NULL);
    // CasinoLaundering
    kos_term* casinolaundering_type = kos_mk_prop("CasinoLaundering");
    kos_ontology_add_type_definition(ontology, "CasinoLaundering", casinolaundering_type, NULL);
    // RealEstateLaundering
    kos_term* realestatelaundering_type = kos_mk_prop("RealEstateLaundering");
    kos_ontology_add_type_definition(ontology, "RealEstateLaundering", realestatelaundering_type, NULL);
    // ArtLaundering
    kos_term* artlaundering_type = kos_mk_prop("ArtLaundering");
    kos_ontology_add_type_definition(ontology, "ArtLaundering", artlaundering_type, NULL);
    // JewelryLaundering
    kos_term* jewelrylaundering_type = kos_mk_prop("JewelryLaundering");
    kos_ontology_add_type_definition(ontology, "JewelryLaundering", jewelrylaundering_type, NULL);
    // VirtualAssetLaundering
    kos_term* virtualassetlaundering_type = kos_mk_prop("VirtualAssetLaundering");
    kos_ontology_add_type_definition(ontology, "VirtualAssetLaundering", virtualassetlaundering_type, NULL);
    // TradeBasedLaundering
    kos_term* tradebasedlaundering_type = kos_mk_prop("TradeBasedLaundering");
    kos_ontology_add_type_definition(ontology, "TradeBasedLaundering", tradebasedlaundering_type, NULL);
    // InvoiceFraud
    kos_term* invoicefraud_type = kos_mk_prop("InvoiceFraud");
    kos_ontology_add_type_definition(ontology, "InvoiceFraud", invoicefraud_type, NULL);
    // OverInvoicing
    kos_term* overinvoicing_type = kos_mk_prop("OverInvoicing");
    kos_ontology_add_type_definition(ontology, "OverInvoicing", overinvoicing_type, NULL);
    // UnderInvoicing
    kos_term* underinvoicing_type = kos_mk_prop("UnderInvoicing");
    kos_ontology_add_type_definition(ontology, "UnderInvoicing", underinvoicing_type, NULL);
    // FalseInvoicing
    kos_term* falseinvoicing_type = kos_mk_prop("FalseInvoicing");
    kos_ontology_add_type_definition(ontology, "FalseInvoicing", falseinvoicing_type, NULL);
    // RoundTripping
    kos_term* roundtripping_type = kos_mk_prop("RoundTripping");
    kos_ontology_add_type_definition(ontology, "RoundTripping", roundtripping_type, NULL);
    // LoanBack
    kos_term* loanback_type = kos_mk_prop("LoanBack");
    kos_ontology_add_type_definition(ontology, "LoanBack", loanback_type, NULL);
    // GiftLaundering
    kos_term* giftlaundering_type = kos_mk_prop("GiftLaundering");
    kos_ontology_add_type_definition(ontology, "GiftLaundering", giftlaundering_type, NULL);
    // ========== suspicious_activities ==========\n    // UnusualTransactionPattern
    kos_term* unusualtransactionpattern_type = kos_mk_prop("UnusualTransactionPattern");
    kos_ontology_add_type_definition(ontology, "UnusualTransactionPattern", unusualtransactionpattern_type, NULL);
    // RapidMovement
    kos_term* rapidmovement_type = kos_mk_prop("RapidMovement");
    kos_ontology_add_type_definition(ontology, "RapidMovement", rapidmovement_type, NULL);
    // CircularTransactions
    kos_term* circulartransactions_type = kos_mk_prop("CircularTransactions");
    kos_ontology_add_type_definition(ontology, "CircularTransactions", circulartransactions_type, NULL);
    // StructuredTransactions
    kos_term* structuredtransactions_type = kos_mk_prop("StructuredTransactions");
    kos_ontology_add_type_definition(ontology, "StructuredTransactions", structuredtransactions_type, NULL);
    // HighValueWithoutPurpose
    kos_term* highvaluewithoutpurpose_type = kos_mk_prop("HighValueWithoutPurpose");
    kos_ontology_add_type_definition(ontology, "HighValueWithoutPurpose", highvaluewithoutpurpose_type, NULL);
    // CrossBorderNoBusiness
    kos_term* crossbordernobusiness_type = kos_mk_prop("CrossBorderNoBusiness");
    kos_ontology_add_type_definition(ontology, "CrossBorderNoBusiness", crossbordernobusiness_type, NULL);
    // MultipleAccountsSameBeneficiary
    kos_term* multipleaccountssamebeneficiary_type = kos_mk_prop("MultipleAccountsSameBeneficiary");
    kos_ontology_add_type_definition(ontology, "MultipleAccountsSameBeneficiary", multipleaccountssamebeneficiary_type, NULL);
    // FrequentSmallDeposits
    kos_term* frequentsmalldeposits_type = kos_mk_prop("FrequentSmallDeposits");
    kos_ontology_add_type_definition(ontology, "FrequentSmallDeposits", frequentsmalldeposits_type, NULL);
    // LargeCashTransactions
    kos_term* largecashtransactions_type = kos_mk_prop("LargeCashTransactions");
    kos_ontology_add_type_definition(ontology, "LargeCashTransactions", largecashtransactions_type, NULL);
    // TransactionsWithSanctionedCountries
    kos_term* transactionswithsanctionedcountries_type = kos_mk_prop("TransactionsWithSanctionedCountries");
    kos_ontology_add_type_definition(ontology, "TransactionsWithSanctionedCountries", transactionswithsanctionedcountries_type, NULL);
    // TransactionsWithPEP
    kos_term* transactionswithpep_type = kos_mk_prop("TransactionsWithPEP");
    kos_ontology_add_type_definition(ontology, "TransactionsWithPEP", transactionswithpep_type, NULL);
    // ComplexTransactionChain
    kos_term* complextransactionchain_type = kos_mk_prop("ComplexTransactionChain");
    kos_ontology_add_type_definition(ontology, "ComplexTransactionChain", complextransactionchain_type, NULL);
    // TransactionsInconsistentWithProfile
    kos_term* transactionsinconsistentwithprofile_type = kos_mk_prop("TransactionsInconsistentWithProfile");
    kos_ontology_add_type_definition(ontology, "TransactionsInconsistentWithProfile", transactionsinconsistentwithprofile_type, NULL);
    // RapidAccountOpeningClosing
    kos_term* rapidaccountopeningclosing_type = kos_mk_prop("RapidAccountOpeningClosing");
    kos_ontology_add_type_definition(ontology, "RapidAccountOpeningClosing", rapidaccountopeningclosing_type, NULL);
    // UnexplainedWealth
    kos_term* unexplainedwealth_type = kos_mk_prop("UnexplainedWealth");
    kos_ontology_add_type_definition(ontology, "UnexplainedWealth", unexplainedwealth_type, NULL);
    // RoundTripTransactions
    kos_term* roundtriptransactions_type = kos_mk_prop("RoundTripTransactions");
    kos_ontology_add_type_definition(ontology, "RoundTripTransactions", roundtriptransactions_type, NULL);
    // BackToBackLoans
    kos_term* backtobackloans_type = kos_mk_prop("BackToBackLoans");
    kos_ontology_add_type_definition(ontology, "BackToBackLoans", backtobackloans_type, NULL);
    // FalseIdentity
    kos_term* falseidentity_type = kos_mk_prop("FalseIdentity");
    kos_ontology_add_type_definition(ontology, "FalseIdentity", falseidentity_type, NULL);
    // IdentityTheft
    kos_term* identitytheft_type = kos_mk_prop("IdentityTheft");
    kos_ontology_add_type_definition(ontology, "IdentityTheft", identitytheft_type, NULL);
    // AccountTakeover
    kos_term* accounttakeover_type = kos_mk_prop("AccountTakeover");
    kos_ontology_add_type_definition(ontology, "AccountTakeover", accounttakeover_type, NULL);
    // SyntheticIdentity
    kos_term* syntheticidentity_type = kos_mk_prop("SyntheticIdentity");
    kos_ontology_add_type_definition(ontology, "SyntheticIdentity", syntheticidentity_type, NULL);
    // MuleAccount
    kos_term* muleaccount_type = kos_mk_prop("MuleAccount");
    kos_ontology_add_type_definition(ontology, "MuleAccount", muleaccount_type, NULL);
    // FunnelAccount
    kos_term* funnelaccount_type = kos_mk_prop("FunnelAccount");
    kos_ontology_add_type_definition(ontology, "FunnelAccount", funnelaccount_type, NULL);
    // SmurfingPattern
    kos_term* smurfingpattern_type = kos_mk_prop("SmurfingPattern");
    kos_ontology_add_type_definition(ontology, "SmurfingPattern", smurfingpattern_type, NULL);
    // StructuringPattern
    kos_term* structuringpattern_type = kos_mk_prop("StructuringPattern");
    kos_ontology_add_type_definition(ontology, "StructuringPattern", structuringpattern_type, NULL);
    // LayeringPattern
    kos_term* layeringpattern_type = kos_mk_prop("LayeringPattern");
    kos_ontology_add_type_definition(ontology, "LayeringPattern", layeringpattern_type, NULL);
    // IntegrationPattern
    kos_term* integrationpattern_type = kos_mk_prop("IntegrationPattern");
    kos_ontology_add_type_definition(ontology, "IntegrationPattern", integrationpattern_type, NULL);
    // ========== regulatory_bodies ==========\n    // FINCEN
    kos_term* fincen_type = kos_mk_prop("FINCEN");
    kos_ontology_add_type_definition(ontology, "FINCEN", fincen_type, NULL);
    // SEC
    kos_term* sec_type = kos_mk_prop("SEC");
    kos_ontology_add_type_definition(ontology, "SEC", sec_type, NULL);
    // CFTC
    kos_term* cftc_type = kos_mk_prop("CFTC");
    kos_ontology_add_type_definition(ontology, "CFTC", cftc_type, NULL);
    // OCC
    kos_term* occ_type = kos_mk_prop("OCC");
    kos_ontology_add_type_definition(ontology, "OCC", occ_type, NULL);
    // FDIC
    kos_term* fdic_type = kos_mk_prop("FDIC");
    kos_ontology_add_type_definition(ontology, "FDIC", fdic_type, NULL);
    // FRB
    kos_term* frb_type = kos_mk_prop("FRB");
    kos_ontology_add_type_definition(ontology, "FRB", frb_type, NULL);
    // FCA
    kos_term* fca_type = kos_mk_prop("FCA");
    kos_ontology_add_type_definition(ontology, "FCA", fca_type, NULL);
    // PRA
    kos_term* pra_type = kos_mk_prop("PRA");
    kos_ontology_add_type_definition(ontology, "PRA", pra_type, NULL);
    // ECB
    kos_term* ecb_type = kos_mk_prop("ECB");
    kos_ontology_add_type_definition(ontology, "ECB", ecb_type, NULL);
    // ESMA
    kos_term* esma_type = kos_mk_prop("ESMA");
    kos_ontology_add_type_definition(ontology, "ESMA", esma_type, NULL);
    // BaFin
    kos_term* bafin_type = kos_mk_prop("BaFin");
    kos_ontology_add_type_definition(ontology, "BaFin", bafin_type, NULL);
    // AMF
    kos_term* amf_type = kos_mk_prop("AMF");
    kos_ontology_add_type_definition(ontology, "AMF", amf_type, NULL);
    // CONSOB
    kos_term* consob_type = kos_mk_prop("CONSOB");
    kos_ontology_add_type_definition(ontology, "CONSOB", consob_type, NULL);
    // CNMV
    kos_term* cnmv_type = kos_mk_prop("CNMV");
    kos_ontology_add_type_definition(ontology, "CNMV", cnmv_type, NULL);
    // AFM
    kos_term* afm_type = kos_mk_prop("AFM");
    kos_ontology_add_type_definition(ontology, "AFM", afm_type, NULL);
    // FINMA
    kos_term* finma_type = kos_mk_prop("FINMA");
    kos_ontology_add_type_definition(ontology, "FINMA", finma_type, NULL);
    // MAS
    kos_term* mas_type = kos_mk_prop("MAS");
    kos_ontology_add_type_definition(ontology, "MAS", mas_type, NULL);
    // HKMA
    kos_term* hkma_type = kos_mk_prop("HKMA");
    kos_ontology_add_type_definition(ontology, "HKMA", hkma_type, NULL);
    // ASIC
    kos_term* asic_type = kos_mk_prop("ASIC");
    kos_ontology_add_type_definition(ontology, "ASIC", asic_type, NULL);
    // OSFI
    kos_term* osfi_type = kos_mk_prop("OSFI");
    kos_ontology_add_type_definition(ontology, "OSFI", osfi_type, NULL);
    // CNBV
    kos_term* cnbv_type = kos_mk_prop("CNBV");
    kos_ontology_add_type_definition(ontology, "CNBV", cnbv_type, NULL);
    // CVM
    kos_term* cvm_type = kos_mk_prop("CVM");
    kos_ontology_add_type_definition(ontology, "CVM", cvm_type, NULL);
    // CBRF
    kos_term* cbrf_type = kos_mk_prop("CBRF");
    kos_ontology_add_type_definition(ontology, "CBRF", cbrf_type, NULL);
    // FSA
    kos_term* fsa_type = kos_mk_prop("FSA");
    kos_ontology_add_type_definition(ontology, "FSA", fsa_type, NULL);
    // FSC
    kos_term* fsc_type = kos_mk_prop("FSC");
    kos_ontology_add_type_definition(ontology, "FSC", fsc_type, NULL);
    // SFC
    kos_term* sfc_type = kos_mk_prop("SFC");
    kos_ontology_add_type_definition(ontology, "SFC", sfc_type, NULL);
    // ASIC (already added, skipping duplicate)\n    // FMA
    kos_term* fma_type = kos_mk_prop("FMA");
    kos_ontology_add_type_definition(ontology, "FMA", fma_type, NULL);
    // FSCL
    kos_term* fscl_type = kos_mk_prop("FSCL");
    kos_ontology_add_type_definition(ontology, "FSCL", fscl_type, NULL);
    // FSP
    kos_term* fsp_type = kos_mk_prop("FSP");
    kos_ontology_add_type_definition(ontology, "FSP", fsp_type, NULL);
    // FMA (already added, skipping duplicate)\n    // ========== document_types ==========\n    // Passport
    kos_term* passport_type = kos_mk_prop("Passport");
    kos_ontology_add_type_definition(ontology, "Passport", passport_type, NULL);
    // NationalID
    kos_term* nationalid_type = kos_mk_prop("NationalID");
    kos_ontology_add_type_definition(ontology, "NationalID", nationalid_type, NULL);
    // DriversLicense
    kos_term* driverslicense_type = kos_mk_prop("DriversLicense");
    kos_ontology_add_type_definition(ontology, "DriversLicense", driverslicense_type, NULL);
    // BirthCertificate
    kos_term* birthcertificate_type = kos_mk_prop("BirthCertificate");
    kos_ontology_add_type_definition(ontology, "BirthCertificate", birthcertificate_type, NULL);
    // ProofOfAddress
    kos_term* proofofaddress_type = kos_mk_prop("ProofOfAddress");
    kos_ontology_add_type_definition(ontology, "ProofOfAddress", proofofaddress_type, NULL);
    // BankStatement
    kos_term* bankstatement_type = kos_mk_prop("BankStatement");
    kos_ontology_add_type_definition(ontology, "BankStatement", bankstatement_type, NULL);
    // TaxReturn
    kos_term* taxreturn_type = kos_mk_prop("TaxReturn");
    kos_ontology_add_type_definition(ontology, "TaxReturn", taxreturn_type, NULL);
    // EmploymentLetter
    kos_term* employmentletter_type = kos_mk_prop("EmploymentLetter");
    kos_ontology_add_type_definition(ontology, "EmploymentLetter", employmentletter_type, NULL);
    // BusinessLicense
    kos_term* businesslicense_type = kos_mk_prop("BusinessLicense");
    kos_ontology_add_type_definition(ontology, "BusinessLicense", businesslicense_type, NULL);
    // CertificateOfIncorporation
    kos_term* certificateofincorporation_type = kos_mk_prop("CertificateOfIncorporation");
    kos_ontology_add_type_definition(ontology, "CertificateOfIncorporation", certificateofincorporation_type, NULL);
    // ArticlesOfAssociation
    kos_term* articlesofassociation_type = kos_mk_prop("ArticlesOfAssociation");
    kos_ontology_add_type_definition(ontology, "ArticlesOfAssociation", articlesofassociation_type, NULL);
    // ShareCertificate
    kos_term* sharecertificate_type = kos_mk_prop("ShareCertificate");
    kos_ontology_add_type_definition(ontology, "ShareCertificate", sharecertificate_type, NULL);
    // PowerOfAttorney
    kos_term* powerofattorney_type = kos_mk_prop("PowerOfAttorney");
    kos_ontology_add_type_definition(ontology, "PowerOfAttorney", powerofattorney_type, NULL);
    // TrustDeed
    kos_term* trustdeed_type = kos_mk_prop("TrustDeed");
    kos_ontology_add_type_definition(ontology, "TrustDeed", trustdeed_type, NULL);
    // BeneficialOwnership
    kos_term* beneficialownership_type = kos_mk_prop("BeneficialOwnership");
    kos_ontology_add_type_definition(ontology, "BeneficialOwnership", beneficialownership_type, NULL);
    // UtilityBill
    kos_term* utilitybill_type = kos_mk_prop("UtilityBill");
    kos_ontology_add_type_definition(ontology, "UtilityBill", utilitybill_type, NULL);
    // RentalAgreement
    kos_term* rentalagreement_type = kos_mk_prop("RentalAgreement");
    kos_ontology_add_type_definition(ontology, "RentalAgreement", rentalagreement_type, NULL);
    // MortgageStatement
    kos_term* mortgagestatement_type = kos_mk_prop("MortgageStatement");
    kos_ontology_add_type_definition(ontology, "MortgageStatement", mortgagestatement_type, NULL);
    // CreditReport
    kos_term* creditreport_type = kos_mk_prop("CreditReport");
    kos_ontology_add_type_definition(ontology, "CreditReport", creditreport_type, NULL);
    // FinancialStatement
    kos_term* financialstatement_type = kos_mk_prop("FinancialStatement");
    kos_ontology_add_type_definition(ontology, "FinancialStatement", financialstatement_type, NULL);
    // AuditReport
    kos_term* auditreport_type = kos_mk_prop("AuditReport");
    kos_ontology_add_type_definition(ontology, "AuditReport", auditreport_type, NULL);
    // ComplianceCertificate
    kos_term* compliancecertificate_type = kos_mk_prop("ComplianceCertificate");
    kos_ontology_add_type_definition(ontology, "ComplianceCertificate", compliancecertificate_type, NULL);
    // KYCForm
    kos_term* kycform_type = kos_mk_prop("KYCForm");
    kos_ontology_add_type_definition(ontology, "KYCForm", kycform_type, NULL);
    // ========== occupations ==========\n    // Banker
    kos_term* banker_type = kos_mk_prop("Banker");
    kos_ontology_add_type_definition(ontology, "Banker", banker_type, NULL);
    // Trader
    kos_term* trader_type = kos_mk_prop("Trader");
    kos_ontology_add_type_definition(ontology, "Trader", trader_type, NULL);
    // Broker
    kos_term* broker_type = kos_mk_prop("Broker");
    kos_ontology_add_type_definition(ontology, "Broker", broker_type, NULL);
    // Accountant
    kos_term* accountant_type = kos_mk_prop("Accountant");
    kos_ontology_add_type_definition(ontology, "Accountant", accountant_type, NULL);
    // Lawyer
    kos_term* lawyer_type = kos_mk_prop("Lawyer");
    kos_ontology_add_type_definition(ontology, "Lawyer", lawyer_type, NULL);
    // Consultant
    kos_term* consultant_type = kos_mk_prop("Consultant");
    kos_ontology_add_type_definition(ontology, "Consultant", consultant_type, NULL);
    // RealEstateAgent
    kos_term* realestateagent_type = kos_mk_prop("RealEstateAgent");
    kos_ontology_add_type_definition(ontology, "RealEstateAgent", realestateagent_type, NULL);
    // JewelryDealer
    kos_term* jewelrydealer_type = kos_mk_prop("JewelryDealer");
    kos_ontology_add_type_definition(ontology, "JewelryDealer", jewelrydealer_type, NULL);
    // ArtDealer
    kos_term* artdealer_type = kos_mk_prop("ArtDealer");
    kos_ontology_add_type_definition(ontology, "ArtDealer", artdealer_type, NULL);
    // CasinoOperator
    kos_term* casinooperator_type = kos_mk_prop("CasinoOperator");
    kos_ontology_add_type_definition(ontology, "CasinoOperator", casinooperator_type, NULL);
    // MoneyServiceOperator
    kos_term* moneyserviceoperator_type = kos_mk_prop("MoneyServiceOperator");
    kos_ontology_add_type_definition(ontology, "MoneyServiceOperator", moneyserviceoperator_type, NULL);
    // CryptocurrencyExchanger
    kos_term* cryptocurrencyexchanger_type = kos_mk_prop("CryptocurrencyExchanger");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyExchanger", cryptocurrencyexchanger_type, NULL);
    // Politician
    kos_term* politician_type = kos_mk_prop("Politician");
    kos_ontology_add_type_definition(ontology, "Politician", politician_type, NULL);
    // GovernmentOfficial
    kos_term* governmentofficial_type = kos_mk_prop("GovernmentOfficial");
    kos_ontology_add_type_definition(ontology, "GovernmentOfficial", governmentofficial_type, NULL);
    // Diplomat
    kos_term* diplomat_type = kos_mk_prop("Diplomat");
    kos_ontology_add_type_definition(ontology, "Diplomat", diplomat_type, NULL);
    // MilitaryOfficer
    kos_term* militaryofficer_type = kos_mk_prop("MilitaryOfficer");
    kos_ontology_add_type_definition(ontology, "MilitaryOfficer", militaryofficer_type, NULL);
    // Judge
    kos_term* judge_type = kos_mk_prop("Judge");
    kos_ontology_add_type_definition(ontology, "Judge", judge_type, NULL);
    // Prosecutor
    kos_term* prosecutor_type = kos_mk_prop("Prosecutor");
    kos_ontology_add_type_definition(ontology, "Prosecutor", prosecutor_type, NULL);
    // PoliceOfficer
    kos_term* policeofficer_type = kos_mk_prop("PoliceOfficer");
    kos_ontology_add_type_definition(ontology, "PoliceOfficer", policeofficer_type, NULL);
    // Doctor
    kos_term* doctor_type = kos_mk_prop("Doctor");
    kos_ontology_add_type_definition(ontology, "Doctor", doctor_type, NULL);
    // Engineer
    kos_term* engineer_type = kos_mk_prop("Engineer");
    kos_ontology_add_type_definition(ontology, "Engineer", engineer_type, NULL);
    // Architect
    kos_term* architect_type = kos_mk_prop("Architect");
    kos_ontology_add_type_definition(ontology, "Architect", architect_type, NULL);
    // Teacher
    kos_term* teacher_type = kos_mk_prop("Teacher");
    kos_ontology_add_type_definition(ontology, "Teacher", teacher_type, NULL);
    // Professor
    kos_term* professor_type = kos_mk_prop("Professor");
    kos_ontology_add_type_definition(ontology, "Professor", professor_type, NULL);
    // Researcher
    kos_term* researcher_type = kos_mk_prop("Researcher");
    kos_ontology_add_type_definition(ontology, "Researcher", researcher_type, NULL);
    // CEO
    kos_term* ceo_type = kos_mk_prop("CEO");
    kos_ontology_add_type_definition(ontology, "CEO", ceo_type, NULL);
    // CFO
    kos_term* cfo_type = kos_mk_prop("CFO");
    kos_ontology_add_type_definition(ontology, "CFO", cfo_type, NULL);
    // CTO
    kos_term* cto_type = kos_mk_prop("CTO");
    kos_ontology_add_type_definition(ontology, "CTO", cto_type, NULL);
    // Director
    kos_term* director_type = kos_mk_prop("Director");
    kos_ontology_add_type_definition(ontology, "Director", director_type, NULL);
    // Manager
    kos_term* manager_type = kos_mk_prop("Manager");
    kos_ontology_add_type_definition(ontology, "Manager", manager_type, NULL);
    // Executive
    kos_term* executive_type = kos_mk_prop("Executive");
    kos_ontology_add_type_definition(ontology, "Executive", executive_type, NULL);
    // Entrepreneur
    kos_term* entrepreneur_type = kos_mk_prop("Entrepreneur");
    kos_ontology_add_type_definition(ontology, "Entrepreneur", entrepreneur_type, NULL);
    // ========== industries ==========\n    // Banking
    kos_term* banking_type = kos_mk_prop("Banking");
    kos_ontology_add_type_definition(ontology, "Banking", banking_type, NULL);
    // Securities
    kos_term* securities_type = kos_mk_prop("Securities");
    kos_ontology_add_type_definition(ontology, "Securities", securities_type, NULL);
    // Insurance
    kos_term* insurance_type = kos_mk_prop("Insurance");
    kos_ontology_add_type_definition(ontology, "Insurance", insurance_type, NULL);
    // RealEstate
    kos_term* realestate_type = kos_mk_prop("RealEstate");
    kos_ontology_add_type_definition(ontology, "RealEstate", realestate_type, NULL);
    // Construction
    kos_term* construction_type = kos_mk_prop("Construction");
    kos_ontology_add_type_definition(ontology, "Construction", construction_type, NULL);
    // Manufacturing
    kos_term* manufacturing_type = kos_mk_prop("Manufacturing");
    kos_ontology_add_type_definition(ontology, "Manufacturing", manufacturing_type, NULL);
    // Retail
    kos_term* retail_type = kos_mk_prop("Retail");
    kos_ontology_add_type_definition(ontology, "Retail", retail_type, NULL);
    // Wholesale
    kos_term* wholesale_type = kos_mk_prop("Wholesale");
    kos_ontology_add_type_definition(ontology, "Wholesale", wholesale_type, NULL);
    // ImportExport
    kos_term* importexport_type = kos_mk_prop("ImportExport");
    kos_ontology_add_type_definition(ontology, "ImportExport", importexport_type, NULL);
    // Tourism
    kos_term* tourism_type = kos_mk_prop("Tourism");
    kos_ontology_add_type_definition(ontology, "Tourism", tourism_type, NULL);
    // Gaming
    kos_term* gaming_type = kos_mk_prop("Gaming");
    kos_ontology_add_type_definition(ontology, "Gaming", gaming_type, NULL);
    // Entertainment
    kos_term* entertainment_type = kos_mk_prop("Entertainment");
    kos_ontology_add_type_definition(ontology, "Entertainment", entertainment_type, NULL);
    // Media
    kos_term* media_type = kos_mk_prop("Media");
    kos_ontology_add_type_definition(ontology, "Media", media_type, NULL);
    // Technology
    kos_term* technology_type = kos_mk_prop("Technology");
    kos_ontology_add_type_definition(ontology, "Technology", technology_type, NULL);
    // Healthcare
    kos_term* healthcare_type = kos_mk_prop("Healthcare");
    kos_ontology_add_type_definition(ontology, "Healthcare", healthcare_type, NULL);
    // Education
    kos_term* education_type = kos_mk_prop("Education");
    kos_ontology_add_type_definition(ontology, "Education", education_type, NULL);
    // NonProfit
    kos_term* nonprofit_type = kos_mk_prop("NonProfit");
    kos_ontology_add_type_definition(ontology, "NonProfit", nonprofit_type, NULL);
    // ReligiousOrganization
    kos_term* religiousorganization_type = kos_mk_prop("ReligiousOrganization");
    kos_ontology_add_type_definition(ontology, "ReligiousOrganization", religiousorganization_type, NULL);
    // Charity
    kos_term* charity_type = kos_mk_prop("Charity");
    kos_ontology_add_type_definition(ontology, "Charity", charity_type, NULL);
    // Agriculture
    kos_term* agriculture_type = kos_mk_prop("Agriculture");
    kos_ontology_add_type_definition(ontology, "Agriculture", agriculture_type, NULL);
    // Mining
    kos_term* mining_type = kos_mk_prop("Mining");
    kos_ontology_add_type_definition(ontology, "Mining", mining_type, NULL);
    // Energy
    kos_term* energy_type = kos_mk_prop("Energy");
    kos_ontology_add_type_definition(ontology, "Energy", energy_type, NULL);
    // Transportation
    kos_term* transportation_type = kos_mk_prop("Transportation");
    kos_ontology_add_type_definition(ontology, "Transportation", transportation_type, NULL);
    // Logistics
    kos_term* logistics_type = kos_mk_prop("Logistics");
    kos_ontology_add_type_definition(ontology, "Logistics", logistics_type, NULL);
    // Telecommunications
    kos_term* telecommunications_type = kos_mk_prop("Telecommunications");
    kos_ontology_add_type_definition(ontology, "Telecommunications", telecommunications_type, NULL);
    // Utilities
    kos_term* utilities_type = kos_mk_prop("Utilities");
    kos_ontology_add_type_definition(ontology, "Utilities", utilities_type, NULL);
    // FoodService
    kos_term* foodservice_type = kos_mk_prop("FoodService");
    kos_ontology_add_type_definition(ontology, "FoodService", foodservice_type, NULL);
    // Hospitality
    kos_term* hospitality_type = kos_mk_prop("Hospitality");
    kos_ontology_add_type_definition(ontology, "Hospitality", hospitality_type, NULL);
    // ========== amount_ranges ==========\n    // MicroTransaction
    kos_term* microtransaction_type = kos_mk_prop("MicroTransaction");
    kos_ontology_add_type_definition(ontology, "MicroTransaction", microtransaction_type, NULL);
    // SmallTransaction
    kos_term* smalltransaction_type = kos_mk_prop("SmallTransaction");
    kos_ontology_add_type_definition(ontology, "SmallTransaction", smalltransaction_type, NULL);
    // MediumTransaction
    kos_term* mediumtransaction_type = kos_mk_prop("MediumTransaction");
    kos_ontology_add_type_definition(ontology, "MediumTransaction", mediumtransaction_type, NULL);
    // LargeTransaction
    kos_term* largetransaction_type = kos_mk_prop("LargeTransaction");
    kos_ontology_add_type_definition(ontology, "LargeTransaction", largetransaction_type, NULL);
    // VeryLargeTransaction
    kos_term* verylargetransaction_type = kos_mk_prop("VeryLargeTransaction");
    kos_ontology_add_type_definition(ontology, "VeryLargeTransaction", verylargetransaction_type, NULL);
    // MegaTransaction
    kos_term* megatransaction_type = kos_mk_prop("MegaTransaction");
    kos_ontology_add_type_definition(ontology, "MegaTransaction", megatransaction_type, NULL);
    // BelowReportingThreshold
    kos_term* belowreportingthreshold_type = kos_mk_prop("BelowReportingThreshold");
    kos_ontology_add_type_definition(ontology, "BelowReportingThreshold", belowreportingthreshold_type, NULL);
    // AboveReportingThreshold
    kos_term* abovereportingthreshold_type = kos_mk_prop("AboveReportingThreshold");
    kos_ontology_add_type_definition(ontology, "AboveReportingThreshold", abovereportingthreshold_type, NULL);
    // StructuredAmount
    kos_term* structuredamount_type = kos_mk_prop("StructuredAmount");
    kos_ontology_add_type_definition(ontology, "StructuredAmount", structuredamount_type, NULL);
    // RoundAmount
    kos_term* roundamount_type = kos_mk_prop("RoundAmount");
    kos_ontology_add_type_definition(ontology, "RoundAmount", roundamount_type, NULL);
    // ========== transaction_frequencies ==========\n    // OneTime
    kos_term* onetime_type = kos_mk_prop("OneTime");
    kos_ontology_add_type_definition(ontology, "OneTime", onetime_type, NULL);
    // Occasional
    kos_term* occasional_type = kos_mk_prop("Occasional");
    kos_ontology_add_type_definition(ontology, "Occasional", occasional_type, NULL);
    // Regular
    kos_term* regular_type = kos_mk_prop("Regular");
    kos_ontology_add_type_definition(ontology, "Regular", regular_type, NULL);
    // Frequent
    kos_term* frequent_type = kos_mk_prop("Frequent");
    kos_ontology_add_type_definition(ontology, "Frequent", frequent_type, NULL);
    // VeryFrequent
    kos_term* veryfrequent_type = kos_mk_prop("VeryFrequent");
    kos_ontology_add_type_definition(ontology, "VeryFrequent", veryfrequent_type, NULL);
    // Daily
    kos_term* daily_type = kos_mk_prop("Daily");
    kos_ontology_add_type_definition(ontology, "Daily", daily_type, NULL);
    // Weekly
    kos_term* weekly_type = kos_mk_prop("Weekly");
    kos_ontology_add_type_definition(ontology, "Weekly", weekly_type, NULL);
    // Monthly
    kos_term* monthly_type = kos_mk_prop("Monthly");
    kos_ontology_add_type_definition(ontology, "Monthly", monthly_type, NULL);
    // Quarterly
    kos_term* quarterly_type = kos_mk_prop("Quarterly");
    kos_ontology_add_type_definition(ontology, "Quarterly", quarterly_type, NULL);
    // Yearly
    kos_term* yearly_type = kos_mk_prop("Yearly");
    kos_ontology_add_type_definition(ontology, "Yearly", yearly_type, NULL);
    // Irregular
    kos_term* irregular_type = kos_mk_prop("Irregular");
    kos_ontology_add_type_definition(ontology, "Irregular", irregular_type, NULL);
    // SuspiciousFrequency
    kos_term* suspiciousfrequency_type = kos_mk_prop("SuspiciousFrequency");
    kos_ontology_add_type_definition(ontology, "SuspiciousFrequency", suspiciousfrequency_type, NULL);
    // ========== account_relationships ==========\n    // SameOwner
    kos_term* sameowner_type = kos_mk_prop("SameOwner");
    kos_ontology_add_type_definition(ontology, "SameOwner", sameowner_type, NULL);
    // RelatedOwner
    kos_term* relatedowner_type = kos_mk_prop("RelatedOwner");
    kos_ontology_add_type_definition(ontology, "RelatedOwner", relatedowner_type, NULL);
    // FamilyMember
    kos_term* familymember_type = kos_mk_prop("FamilyMember");
    kos_ontology_add_type_definition(ontology, "FamilyMember", familymember_type, NULL);
    // BusinessPartner
    kos_term* businesspartner_type = kos_mk_prop("BusinessPartner");
    kos_ontology_add_type_definition(ontology, "BusinessPartner", businesspartner_type, NULL);
    // Subsidiary
    kos_term* subsidiary_type = kos_mk_prop("Subsidiary");
    kos_ontology_add_type_definition(ontology, "Subsidiary", subsidiary_type, NULL);
    // ParentCompany
    kos_term* parentcompany_type = kos_mk_prop("ParentCompany");
    kos_ontology_add_type_definition(ontology, "ParentCompany", parentcompany_type, NULL);
    // Affiliate
    kos_term* affiliate_type = kos_mk_prop("Affiliate");
    kos_ontology_add_type_definition(ontology, "Affiliate", affiliate_type, NULL);
    // Nominee
    kos_term* nominee_type = kos_mk_prop("Nominee");
    kos_ontology_add_type_definition(ontology, "Nominee", nominee_type, NULL);
    // BeneficialOwner
    kos_term* beneficialowner_type = kos_mk_prop("BeneficialOwner");
    kos_ontology_add_type_definition(ontology, "BeneficialOwner", beneficialowner_type, NULL);
    // Trustee
    kos_term* trustee_type = kos_mk_prop("Trustee");
    kos_ontology_add_type_definition(ontology, "Trustee", trustee_type, NULL);
    // Beneficiary
    kos_term* beneficiary_type = kos_mk_prop("Beneficiary");
    kos_ontology_add_type_definition(ontology, "Beneficiary", beneficiary_type, NULL);
    // PowerOfAttorney (already added, skipping duplicate)\n    // AuthorizedSigner
    kos_term* authorizedsigner_type = kos_mk_prop("AuthorizedSigner");
    kos_ontology_add_type_definition(ontology, "AuthorizedSigner", authorizedsigner_type, NULL);
    // JointOwner
    kos_term* jointowner_type = kos_mk_prop("JointOwner");
    kos_ontology_add_type_definition(ontology, "JointOwner", jointowner_type, NULL);
    // NoRelationship
    kos_term* norelationship_type = kos_mk_prop("NoRelationship");
    kos_ontology_add_type_definition(ontology, "NoRelationship", norelationship_type, NULL);
    // ========== transaction_purposes ==========\n    // Salary
    kos_term* salary_type = kos_mk_prop("Salary");
    kos_ontology_add_type_definition(ontology, "Salary", salary_type, NULL);
    // BusinessPayment
    kos_term* businesspayment_type = kos_mk_prop("BusinessPayment");
    kos_ontology_add_type_definition(ontology, "BusinessPayment", businesspayment_type, NULL);
    // Investment
    kos_term* investment_type = kos_mk_prop("Investment");
    kos_ontology_add_type_definition(ontology, "Investment", investment_type, NULL);
    // Loan
    kos_term* loan_type = kos_mk_prop("Loan");
    kos_ontology_add_type_definition(ontology, "Loan", loan_type, NULL);
    // Gift
    kos_term* gift_type = kos_mk_prop("Gift");
    kos_ontology_add_type_definition(ontology, "Gift", gift_type, NULL);
    // Charity (already added, skipping duplicate)\n    // TaxPayment (already added, skipping duplicate)\n    // Purchase
    kos_term* purchase_type = kos_mk_prop("Purchase");
    kos_ontology_add_type_definition(ontology, "Purchase", purchase_type, NULL);
    // Sale
    kos_term* sale_type = kos_mk_prop("Sale");
    kos_ontology_add_type_definition(ontology, "Sale", sale_type, NULL);
    // Refund
    kos_term* refund_type = kos_mk_prop("Refund");
    kos_ontology_add_type_definition(ontology, "Refund", refund_type, NULL);
    // Unknown
    kos_term* unknown_type = kos_mk_prop("Unknown");
    kos_ontology_add_type_definition(ontology, "Unknown", unknown_type, NULL);
    // Suspicious
    kos_term* suspicious_type = kos_mk_prop("Suspicious");
    kos_ontology_add_type_definition(ontology, "Suspicious", suspicious_type, NULL);
    // NoPurpose
    kos_term* nopurpose_type = kos_mk_prop("NoPurpose");
    kos_ontology_add_type_definition(ontology, "NoPurpose", nopurpose_type, NULL);
    // MultiplePurposes
    kos_term* multiplepurposes_type = kos_mk_prop("MultiplePurposes");
    kos_ontology_add_type_definition(ontology, "MultiplePurposes", multiplepurposes_type, NULL);
    // ========== account_statuses ==========\n    // Active
    kos_term* active_type = kos_mk_prop("Active");
    kos_ontology_add_type_definition(ontology, "Active", active_type, NULL);
    // Inactive
    kos_term* inactive_type = kos_mk_prop("Inactive");
    kos_ontology_add_type_definition(ontology, "Inactive", inactive_type, NULL);
    // Dormant
    kos_term* dormant_type = kos_mk_prop("Dormant");
    kos_ontology_add_type_definition(ontology, "Dormant", dormant_type, NULL);
    // Closed
    kos_term* closed_type = kos_mk_prop("Closed");
    kos_ontology_add_type_definition(ontology, "Closed", closed_type, NULL);
    // Frozen (already added, skipping duplicate)\n    // Blocked (already added, skipping duplicate)\n    // UnderInvestigation
    kos_term* underinvestigation_type = kos_mk_prop("UnderInvestigation");
    kos_ontology_add_type_definition(ontology, "UnderInvestigation", underinvestigation_type, NULL);
    // Suspended (already added, skipping duplicate)\n    // Restricted
    kos_term* restricted_type = kos_mk_prop("Restricted");
    kos_ontology_add_type_definition(ontology, "Restricted", restricted_type, NULL);
    // ========== customer_types ==========\n    // Individual
    kos_term* individual_type = kos_mk_prop("Individual");
    kos_ontology_add_type_definition(ontology, "Individual", individual_type, NULL);
    // Corporate
    kos_term* corporate_type = kos_mk_prop("Corporate");
    kos_ontology_add_type_definition(ontology, "Corporate", corporate_type, NULL);
    // Partnership
    kos_term* partnership_type = kos_mk_prop("Partnership");
    kos_ontology_add_type_definition(ontology, "Partnership", partnership_type, NULL);
    // Trust
    kos_term* trust_type = kos_mk_prop("Trust");
    kos_ontology_add_type_definition(ontology, "Trust", trust_type, NULL);
    // Foundation
    kos_term* foundation_type = kos_mk_prop("Foundation");
    kos_ontology_add_type_definition(ontology, "Foundation", foundation_type, NULL);
    // Government
    kos_term* government_type = kos_mk_prop("Government");
    kos_ontology_add_type_definition(ontology, "Government", government_type, NULL);
    // NGO
    kos_term* ngo_type = kos_mk_prop("NGO");
    kos_ontology_add_type_definition(ontology, "NGO", ngo_type, NULL);
    // Charity (already added, skipping duplicate)\n    // ReligiousOrganization (already added, skipping duplicate)\n    // ========== account_channels ==========\n    // Branch
    kos_term* branch_type = kos_mk_prop("Branch");
    kos_ontology_add_type_definition(ontology, "Branch", branch_type, NULL);
    // Online
    kos_term* online_type = kos_mk_prop("Online");
    kos_ontology_add_type_definition(ontology, "Online", online_type, NULL);
    // Mobile
    kos_term* mobile_type = kos_mk_prop("Mobile");
    kos_ontology_add_type_definition(ontology, "Mobile", mobile_type, NULL);
    // ATM
    kos_term* atm_type = kos_mk_prop("ATM");
    kos_ontology_add_type_definition(ontology, "ATM", atm_type, NULL);
    // Phone
    kos_term* phone_type = kos_mk_prop("Phone");
    kos_ontology_add_type_definition(ontology, "Phone", phone_type, NULL);
    // Mail
    kos_term* mail_type = kos_mk_prop("Mail");
    kos_ontology_add_type_definition(ontology, "Mail", mail_type, NULL);
    // Agent
    kos_term* agent_type = kos_mk_prop("Agent");
    kos_ontology_add_type_definition(ontology, "Agent", agent_type, NULL);
    // ThirdParty
    kos_term* thirdparty_type = kos_mk_prop("ThirdParty");
    kos_ontology_add_type_definition(ontology, "ThirdParty", thirdparty_type, NULL);
    // Unknown (already added, skipping duplicate)\n    // ========== transaction_channels ==========\n    // Branch (already added, skipping duplicate)\n    // ATM (already added, skipping duplicate)\n    // Online (already added, skipping duplicate)\n    // Mobile (already added, skipping duplicate)\n    // Phone (already added, skipping duplicate)\n    // Mail (already added, skipping duplicate)\n    // SWIFT
    kos_term* swift_type = kos_mk_prop("SWIFT");
    kos_ontology_add_type_definition(ontology, "SWIFT", swift_type, NULL);
    // ACH
    kos_term* ach_type = kos_mk_prop("ACH");
    kos_ontology_add_type_definition(ontology, "ACH", ach_type, NULL);
    // Card
    kos_term* card_type = kos_mk_prop("Card");
    kos_ontology_add_type_definition(ontology, "Card", card_type, NULL);
    // Cryptocurrency
    kos_term* cryptocurrency_type = kos_mk_prop("Cryptocurrency");
    kos_ontology_add_type_definition(ontology, "Cryptocurrency", cryptocurrency_type, NULL);
    // ========== location_types ==========\n    // Domestic
    kos_term* domestic_type = kos_mk_prop("Domestic");
    kos_ontology_add_type_definition(ontology, "Domestic", domestic_type, NULL);
    // CrossBorder
    kos_term* crossborder_type = kos_mk_prop("CrossBorder");
    kos_ontology_add_type_definition(ontology, "CrossBorder", crossborder_type, NULL);
    // Offshore
    kos_term* offshore_type = kos_mk_prop("Offshore");
    kos_ontology_add_type_definition(ontology, "Offshore", offshore_type, NULL);
    // TaxHaven
    kos_term* taxhaven_type = kos_mk_prop("TaxHaven");
    kos_ontology_add_type_definition(ontology, "TaxHaven", taxhaven_type, NULL);
    // HighRiskJurisdiction
    kos_term* highriskjurisdiction_type = kos_mk_prop("HighRiskJurisdiction");
    kos_ontology_add_type_definition(ontology, "HighRiskJurisdiction", highriskjurisdiction_type, NULL);
    // SanctionedCountry
    kos_term* sanctionedcountry_type = kos_mk_prop("SanctionedCountry");
    kos_ontology_add_type_definition(ontology, "SanctionedCountry", sanctionedcountry_type, NULL);
    // FATFListed
    kos_term* fatflisted_type = kos_mk_prop("FATFListed");
    kos_ontology_add_type_definition(ontology, "FATFListed", fatflisted_type, NULL);
    // ========== time_patterns ==========\n    // BusinessHours
    kos_term* businesshours_type = kos_mk_prop("BusinessHours");
    kos_ontology_add_type_definition(ontology, "BusinessHours", businesshours_type, NULL);
    // AfterHours
    kos_term* afterhours_type = kos_mk_prop("AfterHours");
    kos_ontology_add_type_definition(ontology, "AfterHours", afterhours_type, NULL);
    // Weekend
    kos_term* weekend_type = kos_mk_prop("Weekend");
    kos_ontology_add_type_definition(ontology, "Weekend", weekend_type, NULL);
    // Holiday
    kos_term* holiday_type = kos_mk_prop("Holiday");
    kos_ontology_add_type_definition(ontology, "Holiday", holiday_type, NULL);
    // RapidSequence
    kos_term* rapidsequence_type = kos_mk_prop("RapidSequence");
    kos_ontology_add_type_definition(ontology, "RapidSequence", rapidsequence_type, NULL);
    // SuspiciousTiming
    kos_term* suspicioustiming_type = kos_mk_prop("SuspiciousTiming");
    kos_ontology_add_type_definition(ontology, "SuspiciousTiming", suspicioustiming_type, NULL);
    // UnusualTime
    kos_term* unusualtime_type = kos_mk_prop("UnusualTime");
    kos_ontology_add_type_definition(ontology, "UnusualTime", unusualtime_type, NULL);

    // ========== 复合事件类型 ==========
    // WireTransferEvent: 事件类型
    // WireTransferEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* wiretransferevent_prop = kos_mk_prop("Prop");
    kos_term* wiretransferevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        wiretransferevent_prop);
    kos_term* wiretransferevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        wiretransferevent_sigma_5);
    kos_term* wiretransferevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        wiretransferevent_sigma_4);
    kos_term* wiretransferevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        wiretransferevent_sigma_3);
    kos_term* wiretransferevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        wiretransferevent_sigma_2);
    kos_term* wiretransferevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        wiretransferevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "WireTransferEvent", wiretransferevent_sigma_0, NULL);
    // ACHTransferEvent: 事件类型
    // ACHTransferEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* achtransferevent_prop = kos_mk_prop("Prop");
    kos_term* achtransferevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        achtransferevent_prop);
    kos_term* achtransferevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        achtransferevent_sigma_5);
    kos_term* achtransferevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        achtransferevent_sigma_4);
    kos_term* achtransferevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        achtransferevent_sigma_3);
    kos_term* achtransferevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        achtransferevent_sigma_2);
    kos_term* achtransferevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        achtransferevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "ACHTransferEvent", achtransferevent_sigma_0, NULL);
    // SWIFTTransferEvent: 事件类型
    // SWIFTTransferEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* swifttransferevent_prop = kos_mk_prop("Prop");
    kos_term* swifttransferevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        swifttransferevent_prop);
    kos_term* swifttransferevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        swifttransferevent_sigma_5);
    kos_term* swifttransferevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        swifttransferevent_sigma_4);
    kos_term* swifttransferevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        swifttransferevent_sigma_3);
    kos_term* swifttransferevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        swifttransferevent_sigma_2);
    kos_term* swifttransferevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        swifttransferevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "SWIFTTransferEvent", swifttransferevent_sigma_0, NULL);
    // CashDepositEvent: 事件类型
    // CashDepositEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* cashdepositevent_prop = kos_mk_prop("Prop");
    kos_term* cashdepositevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        cashdepositevent_prop);
    kos_term* cashdepositevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        cashdepositevent_sigma_5);
    kos_term* cashdepositevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        cashdepositevent_sigma_4);
    kos_term* cashdepositevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        cashdepositevent_sigma_3);
    kos_term* cashdepositevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        cashdepositevent_sigma_2);
    kos_term* cashdepositevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        cashdepositevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "CashDepositEvent", cashdepositevent_sigma_0, NULL);
    // CashWithdrawalEvent: 事件类型
    // CashWithdrawalEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* cashwithdrawalevent_prop = kos_mk_prop("Prop");
    kos_term* cashwithdrawalevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        cashwithdrawalevent_prop);
    kos_term* cashwithdrawalevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        cashwithdrawalevent_sigma_5);
    kos_term* cashwithdrawalevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        cashwithdrawalevent_sigma_4);
    kos_term* cashwithdrawalevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        cashwithdrawalevent_sigma_3);
    kos_term* cashwithdrawalevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        cashwithdrawalevent_sigma_2);
    kos_term* cashwithdrawalevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        cashwithdrawalevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "CashWithdrawalEvent", cashwithdrawalevent_sigma_0, NULL);
    // CheckDepositEvent: 事件类型
    // CheckDepositEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* checkdepositevent_prop = kos_mk_prop("Prop");
    kos_term* checkdepositevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        checkdepositevent_prop);
    kos_term* checkdepositevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        checkdepositevent_sigma_5);
    kos_term* checkdepositevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        checkdepositevent_sigma_4);
    kos_term* checkdepositevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        checkdepositevent_sigma_3);
    kos_term* checkdepositevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        checkdepositevent_sigma_2);
    kos_term* checkdepositevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        checkdepositevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "CheckDepositEvent", checkdepositevent_sigma_0, NULL);
    // CheckPaymentEvent: 事件类型
    // CheckPaymentEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* checkpaymentevent_prop = kos_mk_prop("Prop");
    kos_term* checkpaymentevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        checkpaymentevent_prop);
    kos_term* checkpaymentevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        checkpaymentevent_sigma_5);
    kos_term* checkpaymentevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        checkpaymentevent_sigma_4);
    kos_term* checkpaymentevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        checkpaymentevent_sigma_3);
    kos_term* checkpaymentevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        checkpaymentevent_sigma_2);
    kos_term* checkpaymentevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        checkpaymentevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "CheckPaymentEvent", checkpaymentevent_sigma_0, NULL);
    // CreditCardPaymentEvent: 事件类型
    // CreditCardPaymentEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* creditcardpaymentevent_prop = kos_mk_prop("Prop");
    kos_term* creditcardpaymentevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        creditcardpaymentevent_prop);
    kos_term* creditcardpaymentevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        creditcardpaymentevent_sigma_5);
    kos_term* creditcardpaymentevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        creditcardpaymentevent_sigma_4);
    kos_term* creditcardpaymentevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        creditcardpaymentevent_sigma_3);
    kos_term* creditcardpaymentevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        creditcardpaymentevent_sigma_2);
    kos_term* creditcardpaymentevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        creditcardpaymentevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "CreditCardPaymentEvent", creditcardpaymentevent_sigma_0, NULL);
    // DebitCardTransactionEvent: 事件类型
    // DebitCardTransactionEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* debitcardtransactionevent_prop = kos_mk_prop("Prop");
    kos_term* debitcardtransactionevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        debitcardtransactionevent_prop);
    kos_term* debitcardtransactionevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        debitcardtransactionevent_sigma_5);
    kos_term* debitcardtransactionevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        debitcardtransactionevent_sigma_4);
    kos_term* debitcardtransactionevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        debitcardtransactionevent_sigma_3);
    kos_term* debitcardtransactionevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        debitcardtransactionevent_sigma_2);
    kos_term* debitcardtransactionevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        debitcardtransactionevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "DebitCardTransactionEvent", debitcardtransactionevent_sigma_0, NULL);
    // OnlinePaymentEvent: 事件类型
    // OnlinePaymentEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* onlinepaymentevent_prop = kos_mk_prop("Prop");
    kos_term* onlinepaymentevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        onlinepaymentevent_prop);
    kos_term* onlinepaymentevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        onlinepaymentevent_sigma_5);
    kos_term* onlinepaymentevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        onlinepaymentevent_sigma_4);
    kos_term* onlinepaymentevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        onlinepaymentevent_sigma_3);
    kos_term* onlinepaymentevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        onlinepaymentevent_sigma_2);
    kos_term* onlinepaymentevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        onlinepaymentevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "OnlinePaymentEvent", onlinepaymentevent_sigma_0, NULL);
    // MobilePaymentEvent: 事件类型
    // MobilePaymentEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* mobilepaymentevent_prop = kos_mk_prop("Prop");
    kos_term* mobilepaymentevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        mobilepaymentevent_prop);
    kos_term* mobilepaymentevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        mobilepaymentevent_sigma_5);
    kos_term* mobilepaymentevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        mobilepaymentevent_sigma_4);
    kos_term* mobilepaymentevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        mobilepaymentevent_sigma_3);
    kos_term* mobilepaymentevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        mobilepaymentevent_sigma_2);
    kos_term* mobilepaymentevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        mobilepaymentevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "MobilePaymentEvent", mobilepaymentevent_sigma_0, NULL);
    // CryptocurrencyTransferEvent: 事件类型
    // CryptocurrencyTransferEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* cryptocurrencytransferevent_prop = kos_mk_prop("Prop");
    kos_term* cryptocurrencytransferevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        cryptocurrencytransferevent_prop);
    kos_term* cryptocurrencytransferevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        cryptocurrencytransferevent_sigma_5);
    kos_term* cryptocurrencytransferevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        cryptocurrencytransferevent_sigma_4);
    kos_term* cryptocurrencytransferevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        cryptocurrencytransferevent_sigma_3);
    kos_term* cryptocurrencytransferevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        cryptocurrencytransferevent_sigma_2);
    kos_term* cryptocurrencytransferevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        cryptocurrencytransferevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransferEvent", cryptocurrencytransferevent_sigma_0, NULL);
    // StockTradeEvent: 事件类型
    // StockTradeEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* stocktradeevent_prop = kos_mk_prop("Prop");
    kos_term* stocktradeevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        stocktradeevent_prop);
    kos_term* stocktradeevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        stocktradeevent_sigma_5);
    kos_term* stocktradeevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        stocktradeevent_sigma_4);
    kos_term* stocktradeevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        stocktradeevent_sigma_3);
    kos_term* stocktradeevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        stocktradeevent_sigma_2);
    kos_term* stocktradeevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        stocktradeevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "StockTradeEvent", stocktradeevent_sigma_0, NULL);
    // BondTradeEvent: 事件类型
    // BondTradeEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* bondtradeevent_prop = kos_mk_prop("Prop");
    kos_term* bondtradeevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        bondtradeevent_prop);
    kos_term* bondtradeevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        bondtradeevent_sigma_5);
    kos_term* bondtradeevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        bondtradeevent_sigma_4);
    kos_term* bondtradeevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        bondtradeevent_sigma_3);
    kos_term* bondtradeevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        bondtradeevent_sigma_2);
    kos_term* bondtradeevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        bondtradeevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "BondTradeEvent", bondtradeevent_sigma_0, NULL);
    // DerivativeTradeEvent: 事件类型
    // DerivativeTradeEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* derivativetradeevent_prop = kos_mk_prop("Prop");
    kos_term* derivativetradeevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        derivativetradeevent_prop);
    kos_term* derivativetradeevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        derivativetradeevent_sigma_5);
    kos_term* derivativetradeevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        derivativetradeevent_sigma_4);
    kos_term* derivativetradeevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        derivativetradeevent_sigma_3);
    kos_term* derivativetradeevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        derivativetradeevent_sigma_2);
    kos_term* derivativetradeevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        derivativetradeevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "DerivativeTradeEvent", derivativetradeevent_sigma_0, NULL);
    // ForexTradeEvent: 事件类型
    // ForexTradeEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* forextradeevent_prop = kos_mk_prop("Prop");
    kos_term* forextradeevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        forextradeevent_prop);
    kos_term* forextradeevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        forextradeevent_sigma_5);
    kos_term* forextradeevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        forextradeevent_sigma_4);
    kos_term* forextradeevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        forextradeevent_sigma_3);
    kos_term* forextradeevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        forextradeevent_sigma_2);
    kos_term* forextradeevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        forextradeevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "ForexTradeEvent", forextradeevent_sigma_0, NULL);
    // CommodityTradeEvent: 事件类型
    // CommodityTradeEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* commoditytradeevent_prop = kos_mk_prop("Prop");
    kos_term* commoditytradeevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        commoditytradeevent_prop);
    kos_term* commoditytradeevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        commoditytradeevent_sigma_5);
    kos_term* commoditytradeevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        commoditytradeevent_sigma_4);
    kos_term* commoditytradeevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        commoditytradeevent_sigma_3);
    kos_term* commoditytradeevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        commoditytradeevent_sigma_2);
    kos_term* commoditytradeevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        commoditytradeevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "CommodityTradeEvent", commoditytradeevent_sigma_0, NULL);
    // OptionsTradeEvent: 事件类型
    // OptionsTradeEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* optionstradeevent_prop = kos_mk_prop("Prop");
    kos_term* optionstradeevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        optionstradeevent_prop);
    kos_term* optionstradeevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        optionstradeevent_sigma_5);
    kos_term* optionstradeevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        optionstradeevent_sigma_4);
    kos_term* optionstradeevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        optionstradeevent_sigma_3);
    kos_term* optionstradeevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        optionstradeevent_sigma_2);
    kos_term* optionstradeevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        optionstradeevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "OptionsTradeEvent", optionstradeevent_sigma_0, NULL);
    // FuturesTradeEvent: 事件类型
    // FuturesTradeEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* futurestradeevent_prop = kos_mk_prop("Prop");
    kos_term* futurestradeevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        futurestradeevent_prop);
    kos_term* futurestradeevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        futurestradeevent_sigma_5);
    kos_term* futurestradeevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        futurestradeevent_sigma_4);
    kos_term* futurestradeevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        futurestradeevent_sigma_3);
    kos_term* futurestradeevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        futurestradeevent_sigma_2);
    kos_term* futurestradeevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        futurestradeevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "FuturesTradeEvent", futurestradeevent_sigma_0, NULL);
    // SwapTransactionEvent: 事件类型
    // SwapTransactionEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* swaptransactionevent_prop = kos_mk_prop("Prop");
    kos_term* swaptransactionevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        swaptransactionevent_prop);
    kos_term* swaptransactionevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        swaptransactionevent_sigma_5);
    kos_term* swaptransactionevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        swaptransactionevent_sigma_4);
    kos_term* swaptransactionevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        swaptransactionevent_sigma_3);
    kos_term* swaptransactionevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        swaptransactionevent_sigma_2);
    kos_term* swaptransactionevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        swaptransactionevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "SwapTransactionEvent", swaptransactionevent_sigma_0, NULL);
    // LoanDisbursementEvent: 事件类型
    // LoanDisbursementEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* loandisbursementevent_prop = kos_mk_prop("Prop");
    kos_term* loandisbursementevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        loandisbursementevent_prop);
    kos_term* loandisbursementevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        loandisbursementevent_sigma_5);
    kos_term* loandisbursementevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        loandisbursementevent_sigma_4);
    kos_term* loandisbursementevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        loandisbursementevent_sigma_3);
    kos_term* loandisbursementevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        loandisbursementevent_sigma_2);
    kos_term* loandisbursementevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        loandisbursementevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "LoanDisbursementEvent", loandisbursementevent_sigma_0, NULL);
    // LoanRepaymentEvent: 事件类型
    // LoanRepaymentEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* loanrepaymentevent_prop = kos_mk_prop("Prop");
    kos_term* loanrepaymentevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        loanrepaymentevent_prop);
    kos_term* loanrepaymentevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        loanrepaymentevent_sigma_5);
    kos_term* loanrepaymentevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        loanrepaymentevent_sigma_4);
    kos_term* loanrepaymentevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        loanrepaymentevent_sigma_3);
    kos_term* loanrepaymentevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        loanrepaymentevent_sigma_2);
    kos_term* loanrepaymentevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        loanrepaymentevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "LoanRepaymentEvent", loanrepaymentevent_sigma_0, NULL);
    // InterestPaymentEvent: 事件类型
    // InterestPaymentEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* interestpaymentevent_prop = kos_mk_prop("Prop");
    kos_term* interestpaymentevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        interestpaymentevent_prop);
    kos_term* interestpaymentevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        interestpaymentevent_sigma_5);
    kos_term* interestpaymentevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        interestpaymentevent_sigma_4);
    kos_term* interestpaymentevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        interestpaymentevent_sigma_3);
    kos_term* interestpaymentevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        interestpaymentevent_sigma_2);
    kos_term* interestpaymentevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        interestpaymentevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "InterestPaymentEvent", interestpaymentevent_sigma_0, NULL);
    // DividendPaymentEvent: 事件类型
    // DividendPaymentEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* dividendpaymentevent_prop = kos_mk_prop("Prop");
    kos_term* dividendpaymentevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        dividendpaymentevent_prop);
    kos_term* dividendpaymentevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        dividendpaymentevent_sigma_5);
    kos_term* dividendpaymentevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        dividendpaymentevent_sigma_4);
    kos_term* dividendpaymentevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        dividendpaymentevent_sigma_3);
    kos_term* dividendpaymentevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        dividendpaymentevent_sigma_2);
    kos_term* dividendpaymentevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        dividendpaymentevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "DividendPaymentEvent", dividendpaymentevent_sigma_0, NULL);
    // InsurancePremiumEvent: 事件类型
    // InsurancePremiumEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* insurancepremiumevent_prop = kos_mk_prop("Prop");
    kos_term* insurancepremiumevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        insurancepremiumevent_prop);
    kos_term* insurancepremiumevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        insurancepremiumevent_sigma_5);
    kos_term* insurancepremiumevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        insurancepremiumevent_sigma_4);
    kos_term* insurancepremiumevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        insurancepremiumevent_sigma_3);
    kos_term* insurancepremiumevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        insurancepremiumevent_sigma_2);
    kos_term* insurancepremiumevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        insurancepremiumevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "InsurancePremiumEvent", insurancepremiumevent_sigma_0, NULL);
    // InsuranceClaimEvent: 事件类型
    // InsuranceClaimEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* insuranceclaimevent_prop = kos_mk_prop("Prop");
    kos_term* insuranceclaimevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        insuranceclaimevent_prop);
    kos_term* insuranceclaimevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        insuranceclaimevent_sigma_5);
    kos_term* insuranceclaimevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        insuranceclaimevent_sigma_4);
    kos_term* insuranceclaimevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        insuranceclaimevent_sigma_3);
    kos_term* insuranceclaimevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        insuranceclaimevent_sigma_2);
    kos_term* insuranceclaimevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        insuranceclaimevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "InsuranceClaimEvent", insuranceclaimevent_sigma_0, NULL);
    // PensionPaymentEvent: 事件类型
    // PensionPaymentEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* pensionpaymentevent_prop = kos_mk_prop("Prop");
    kos_term* pensionpaymentevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        pensionpaymentevent_prop);
    kos_term* pensionpaymentevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        pensionpaymentevent_sigma_5);
    kos_term* pensionpaymentevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        pensionpaymentevent_sigma_4);
    kos_term* pensionpaymentevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        pensionpaymentevent_sigma_3);
    kos_term* pensionpaymentevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        pensionpaymentevent_sigma_2);
    kos_term* pensionpaymentevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        pensionpaymentevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "PensionPaymentEvent", pensionpaymentevent_sigma_0, NULL);
    // SalaryPaymentEvent: 事件类型
    // SalaryPaymentEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* salarypaymentevent_prop = kos_mk_prop("Prop");
    kos_term* salarypaymentevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        salarypaymentevent_prop);
    kos_term* salarypaymentevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        salarypaymentevent_sigma_5);
    kos_term* salarypaymentevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        salarypaymentevent_sigma_4);
    kos_term* salarypaymentevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        salarypaymentevent_sigma_3);
    kos_term* salarypaymentevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        salarypaymentevent_sigma_2);
    kos_term* salarypaymentevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        salarypaymentevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "SalaryPaymentEvent", salarypaymentevent_sigma_0, NULL);
    // TaxPaymentEvent: 事件类型
    // TaxPaymentEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* taxpaymentevent_prop = kos_mk_prop("Prop");
    kos_term* taxpaymentevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        taxpaymentevent_prop);
    kos_term* taxpaymentevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        taxpaymentevent_sigma_5);
    kos_term* taxpaymentevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        taxpaymentevent_sigma_4);
    kos_term* taxpaymentevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        taxpaymentevent_sigma_3);
    kos_term* taxpaymentevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        taxpaymentevent_sigma_2);
    kos_term* taxpaymentevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        taxpaymentevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "TaxPaymentEvent", taxpaymentevent_sigma_0, NULL);
    // RefundPaymentEvent: 事件类型
    // RefundPaymentEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* refundpaymentevent_prop = kos_mk_prop("Prop");
    kos_term* refundpaymentevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        refundpaymentevent_prop);
    kos_term* refundpaymentevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        refundpaymentevent_sigma_5);
    kos_term* refundpaymentevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        refundpaymentevent_sigma_4);
    kos_term* refundpaymentevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        refundpaymentevent_sigma_3);
    kos_term* refundpaymentevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        refundpaymentevent_sigma_2);
    kos_term* refundpaymentevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        refundpaymentevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "RefundPaymentEvent", refundpaymentevent_sigma_0, NULL);
    // GiftTransferEvent: 事件类型
    // GiftTransferEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* gifttransferevent_prop = kos_mk_prop("Prop");
    kos_term* gifttransferevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        gifttransferevent_prop);
    kos_term* gifttransferevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        gifttransferevent_sigma_5);
    kos_term* gifttransferevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        gifttransferevent_sigma_4);
    kos_term* gifttransferevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        gifttransferevent_sigma_3);
    kos_term* gifttransferevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        gifttransferevent_sigma_2);
    kos_term* gifttransferevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        gifttransferevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "GiftTransferEvent", gifttransferevent_sigma_0, NULL);
    // CharityDonationEvent: 事件类型
    // CharityDonationEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* charitydonationevent_prop = kos_mk_prop("Prop");
    kos_term* charitydonationevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        charitydonationevent_prop);
    kos_term* charitydonationevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        charitydonationevent_sigma_5);
    kos_term* charitydonationevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        charitydonationevent_sigma_4);
    kos_term* charitydonationevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        charitydonationevent_sigma_3);
    kos_term* charitydonationevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        charitydonationevent_sigma_2);
    kos_term* charitydonationevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        charitydonationevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "CharityDonationEvent", charitydonationevent_sigma_0, NULL);
    // RealEstatePurchaseEvent: 事件类型
    // RealEstatePurchaseEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* realestatepurchaseevent_prop = kos_mk_prop("Prop");
    kos_term* realestatepurchaseevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        realestatepurchaseevent_prop);
    kos_term* realestatepurchaseevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        realestatepurchaseevent_sigma_5);
    kos_term* realestatepurchaseevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        realestatepurchaseevent_sigma_4);
    kos_term* realestatepurchaseevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        realestatepurchaseevent_sigma_3);
    kos_term* realestatepurchaseevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        realestatepurchaseevent_sigma_2);
    kos_term* realestatepurchaseevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        realestatepurchaseevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "RealEstatePurchaseEvent", realestatepurchaseevent_sigma_0, NULL);
    // RealEstateSaleEvent: 事件类型
    // RealEstateSaleEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* realestatesaleevent_prop = kos_mk_prop("Prop");
    kos_term* realestatesaleevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        realestatesaleevent_prop);
    kos_term* realestatesaleevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        realestatesaleevent_sigma_5);
    kos_term* realestatesaleevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        realestatesaleevent_sigma_4);
    kos_term* realestatesaleevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        realestatesaleevent_sigma_3);
    kos_term* realestatesaleevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        realestatesaleevent_sigma_2);
    kos_term* realestatesaleevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        realestatesaleevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "RealEstateSaleEvent", realestatesaleevent_sigma_0, NULL);
    // ArtPurchaseEvent: 事件类型
    // ArtPurchaseEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* artpurchaseevent_prop = kos_mk_prop("Prop");
    kos_term* artpurchaseevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        artpurchaseevent_prop);
    kos_term* artpurchaseevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        artpurchaseevent_sigma_5);
    kos_term* artpurchaseevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        artpurchaseevent_sigma_4);
    kos_term* artpurchaseevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        artpurchaseevent_sigma_3);
    kos_term* artpurchaseevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        artpurchaseevent_sigma_2);
    kos_term* artpurchaseevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        artpurchaseevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "ArtPurchaseEvent", artpurchaseevent_sigma_0, NULL);
    // ArtSaleEvent: 事件类型
    // ArtSaleEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* artsaleevent_prop = kos_mk_prop("Prop");
    kos_term* artsaleevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        artsaleevent_prop);
    kos_term* artsaleevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        artsaleevent_sigma_5);
    kos_term* artsaleevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        artsaleevent_sigma_4);
    kos_term* artsaleevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        artsaleevent_sigma_3);
    kos_term* artsaleevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        artsaleevent_sigma_2);
    kos_term* artsaleevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        artsaleevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "ArtSaleEvent", artsaleevent_sigma_0, NULL);
    // JewelryPurchaseEvent: 事件类型
    // JewelryPurchaseEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* jewelrypurchaseevent_prop = kos_mk_prop("Prop");
    kos_term* jewelrypurchaseevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        jewelrypurchaseevent_prop);
    kos_term* jewelrypurchaseevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        jewelrypurchaseevent_sigma_5);
    kos_term* jewelrypurchaseevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        jewelrypurchaseevent_sigma_4);
    kos_term* jewelrypurchaseevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        jewelrypurchaseevent_sigma_3);
    kos_term* jewelrypurchaseevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        jewelrypurchaseevent_sigma_2);
    kos_term* jewelrypurchaseevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        jewelrypurchaseevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "JewelryPurchaseEvent", jewelrypurchaseevent_sigma_0, NULL);
    // JewelrySaleEvent: 事件类型
    // JewelrySaleEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* jewelrysaleevent_prop = kos_mk_prop("Prop");
    kos_term* jewelrysaleevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        jewelrysaleevent_prop);
    kos_term* jewelrysaleevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        jewelrysaleevent_sigma_5);
    kos_term* jewelrysaleevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        jewelrysaleevent_sigma_4);
    kos_term* jewelrysaleevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        jewelrysaleevent_sigma_3);
    kos_term* jewelrysaleevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        jewelrysaleevent_sigma_2);
    kos_term* jewelrysaleevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        jewelrysaleevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "JewelrySaleEvent", jewelrysaleevent_sigma_0, NULL);
    // VehiclePurchaseEvent: 事件类型
    // VehiclePurchaseEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* vehiclepurchaseevent_prop = kos_mk_prop("Prop");
    kos_term* vehiclepurchaseevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        vehiclepurchaseevent_prop);
    kos_term* vehiclepurchaseevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        vehiclepurchaseevent_sigma_5);
    kos_term* vehiclepurchaseevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        vehiclepurchaseevent_sigma_4);
    kos_term* vehiclepurchaseevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        vehiclepurchaseevent_sigma_3);
    kos_term* vehiclepurchaseevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        vehiclepurchaseevent_sigma_2);
    kos_term* vehiclepurchaseevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        vehiclepurchaseevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "VehiclePurchaseEvent", vehiclepurchaseevent_sigma_0, NULL);
    // VehicleSaleEvent: 事件类型
    // VehicleSaleEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* vehiclesaleevent_prop = kos_mk_prop("Prop");
    kos_term* vehiclesaleevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        vehiclesaleevent_prop);
    kos_term* vehiclesaleevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        vehiclesaleevent_sigma_5);
    kos_term* vehiclesaleevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        vehiclesaleevent_sigma_4);
    kos_term* vehiclesaleevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        vehiclesaleevent_sigma_3);
    kos_term* vehiclesaleevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        vehiclesaleevent_sigma_2);
    kos_term* vehiclesaleevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        vehiclesaleevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "VehicleSaleEvent", vehiclesaleevent_sigma_0, NULL);
    // CasinoDepositEvent: 事件类型
    // CasinoDepositEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* casinodepositevent_prop = kos_mk_prop("Prop");
    kos_term* casinodepositevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        casinodepositevent_prop);
    kos_term* casinodepositevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        casinodepositevent_sigma_5);
    kos_term* casinodepositevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        casinodepositevent_sigma_4);
    kos_term* casinodepositevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        casinodepositevent_sigma_3);
    kos_term* casinodepositevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        casinodepositevent_sigma_2);
    kos_term* casinodepositevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        casinodepositevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "CasinoDepositEvent", casinodepositevent_sigma_0, NULL);
    // CasinoWithdrawalEvent: 事件类型
    // CasinoWithdrawalEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* casinowithdrawalevent_prop = kos_mk_prop("Prop");
    kos_term* casinowithdrawalevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        casinowithdrawalevent_prop);
    kos_term* casinowithdrawalevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        casinowithdrawalevent_sigma_5);
    kos_term* casinowithdrawalevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        casinowithdrawalevent_sigma_4);
    kos_term* casinowithdrawalevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        casinowithdrawalevent_sigma_3);
    kos_term* casinowithdrawalevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        casinowithdrawalevent_sigma_2);
    kos_term* casinowithdrawalevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        casinowithdrawalevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "CasinoWithdrawalEvent", casinowithdrawalevent_sigma_0, NULL);
    // GamingTransactionEvent: 事件类型
    // GamingTransactionEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* gamingtransactionevent_prop = kos_mk_prop("Prop");
    kos_term* gamingtransactionevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        gamingtransactionevent_prop);
    kos_term* gamingtransactionevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        gamingtransactionevent_sigma_5);
    kos_term* gamingtransactionevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        gamingtransactionevent_sigma_4);
    kos_term* gamingtransactionevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        gamingtransactionevent_sigma_3);
    kos_term* gamingtransactionevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        gamingtransactionevent_sigma_2);
    kos_term* gamingtransactionevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        gamingtransactionevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "GamingTransactionEvent", gamingtransactionevent_sigma_0, NULL);
    // LotteryPaymentEvent: 事件类型
    // LotteryPaymentEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* lotterypaymentevent_prop = kos_mk_prop("Prop");
    kos_term* lotterypaymentevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        lotterypaymentevent_prop);
    kos_term* lotterypaymentevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        lotterypaymentevent_sigma_5);
    kos_term* lotterypaymentevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        lotterypaymentevent_sigma_4);
    kos_term* lotterypaymentevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        lotterypaymentevent_sigma_3);
    kos_term* lotterypaymentevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        lotterypaymentevent_sigma_2);
    kos_term* lotterypaymentevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        lotterypaymentevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "LotteryPaymentEvent", lotterypaymentevent_sigma_0, NULL);
    // HawalaTransferEvent: 事件类型
    // HawalaTransferEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* hawalatransferevent_prop = kos_mk_prop("Prop");
    kos_term* hawalatransferevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        hawalatransferevent_prop);
    kos_term* hawalatransferevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        hawalatransferevent_sigma_5);
    kos_term* hawalatransferevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        hawalatransferevent_sigma_4);
    kos_term* hawalatransferevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        hawalatransferevent_sigma_3);
    kos_term* hawalatransferevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        hawalatransferevent_sigma_2);
    kos_term* hawalatransferevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        hawalatransferevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "HawalaTransferEvent", hawalatransferevent_sigma_0, NULL);
    // UndergroundBankingTransferEvent: 事件类型
    // UndergroundBankingTransferEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* undergroundbankingtransferevent_prop = kos_mk_prop("Prop");
    kos_term* undergroundbankingtransferevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        undergroundbankingtransferevent_prop);
    kos_term* undergroundbankingtransferevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        undergroundbankingtransferevent_sigma_5);
    kos_term* undergroundbankingtransferevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        undergroundbankingtransferevent_sigma_4);
    kos_term* undergroundbankingtransferevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        undergroundbankingtransferevent_sigma_3);
    kos_term* undergroundbankingtransferevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        undergroundbankingtransferevent_sigma_2);
    kos_term* undergroundbankingtransferevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        undergroundbankingtransferevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "UndergroundBankingTransferEvent", undergroundbankingtransferevent_sigma_0, NULL);
    // MoneyOrderEvent: 事件类型
    // MoneyOrderEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* moneyorderevent_prop = kos_mk_prop("Prop");
    kos_term* moneyorderevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        moneyorderevent_prop);
    kos_term* moneyorderevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        moneyorderevent_sigma_5);
    kos_term* moneyorderevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        moneyorderevent_sigma_4);
    kos_term* moneyorderevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        moneyorderevent_sigma_3);
    kos_term* moneyorderevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        moneyorderevent_sigma_2);
    kos_term* moneyorderevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        moneyorderevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "MoneyOrderEvent", moneyorderevent_sigma_0, NULL);
    // TravelersCheckEvent: 事件类型
    // TravelersCheckEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* travelerscheckevent_prop = kos_mk_prop("Prop");
    kos_term* travelerscheckevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        travelerscheckevent_prop);
    kos_term* travelerscheckevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        travelerscheckevent_sigma_5);
    kos_term* travelerscheckevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        travelerscheckevent_sigma_4);
    kos_term* travelerscheckevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        travelerscheckevent_sigma_3);
    kos_term* travelerscheckevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        travelerscheckevent_sigma_2);
    kos_term* travelerscheckevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        travelerscheckevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "TravelersCheckEvent", travelerscheckevent_sigma_0, NULL);
    // PrepaidCardLoadEvent: 事件类型
    // PrepaidCardLoadEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* prepaidcardloadevent_prop = kos_mk_prop("Prop");
    kos_term* prepaidcardloadevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        prepaidcardloadevent_prop);
    kos_term* prepaidcardloadevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        prepaidcardloadevent_sigma_5);
    kos_term* prepaidcardloadevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        prepaidcardloadevent_sigma_4);
    kos_term* prepaidcardloadevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        prepaidcardloadevent_sigma_3);
    kos_term* prepaidcardloadevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        prepaidcardloadevent_sigma_2);
    kos_term* prepaidcardloadevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        prepaidcardloadevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "PrepaidCardLoadEvent", prepaidcardloadevent_sigma_0, NULL);
    // PrepaidCardSpendEvent: 事件类型
    // PrepaidCardSpendEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* prepaidcardspendevent_prop = kos_mk_prop("Prop");
    kos_term* prepaidcardspendevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        prepaidcardspendevent_prop);
    kos_term* prepaidcardspendevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        prepaidcardspendevent_sigma_5);
    kos_term* prepaidcardspendevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        prepaidcardspendevent_sigma_4);
    kos_term* prepaidcardspendevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        prepaidcardspendevent_sigma_3);
    kos_term* prepaidcardspendevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        prepaidcardspendevent_sigma_2);
    kos_term* prepaidcardspendevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        prepaidcardspendevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "PrepaidCardSpendEvent", prepaidcardspendevent_sigma_0, NULL);
    // CryptocurrencyExchangeEvent: 事件类型
    // CryptocurrencyExchangeEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* cryptocurrencyexchangeevent_prop = kos_mk_prop("Prop");
    kos_term* cryptocurrencyexchangeevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        cryptocurrencyexchangeevent_prop);
    kos_term* cryptocurrencyexchangeevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        cryptocurrencyexchangeevent_sigma_5);
    kos_term* cryptocurrencyexchangeevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        cryptocurrencyexchangeevent_sigma_4);
    kos_term* cryptocurrencyexchangeevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        cryptocurrencyexchangeevent_sigma_3);
    kos_term* cryptocurrencyexchangeevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        cryptocurrencyexchangeevent_sigma_2);
    kos_term* cryptocurrencyexchangeevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        cryptocurrencyexchangeevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "CryptocurrencyExchangeEvent", cryptocurrencyexchangeevent_sigma_0, NULL);
    // TokenTransferEvent: 事件类型
    // TokenTransferEvent = Σ(tx_id: TransactionID) Σ(from_account: AccountID) Σ(to_account: AccountID) Σ(amount: Amount) Σ(currency: Currency) Σ(timestamp: Time) Prop
    kos_term* tokentransferevent_prop = kos_mk_prop("Prop");
    kos_term* tokentransferevent_sigma_5 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        tokentransferevent_prop);
    kos_term* tokentransferevent_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Currency"),
        tokentransferevent_sigma_5);
    kos_term* tokentransferevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Amount"),
        tokentransferevent_sigma_4);
    kos_term* tokentransferevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        tokentransferevent_sigma_3);
    kos_term* tokentransferevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        tokentransferevent_sigma_2);
    kos_term* tokentransferevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        tokentransferevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "TokenTransferEvent", tokentransferevent_sigma_0, NULL);
    // UnusualTransactionPatternEvent: 事件类型
    // UnusualTransactionPatternEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* unusualtransactionpatternevent_prop = kos_mk_prop("Prop");
    kos_term* unusualtransactionpatternevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        unusualtransactionpatternevent_prop);
    kos_term* unusualtransactionpatternevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        unusualtransactionpatternevent_sigma_3);
    kos_term* unusualtransactionpatternevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        unusualtransactionpatternevent_sigma_2);
    kos_term* unusualtransactionpatternevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        unusualtransactionpatternevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "UnusualTransactionPatternEvent", unusualtransactionpatternevent_sigma_0, NULL);
    // RapidMovementEvent: 事件类型
    // RapidMovementEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* rapidmovementevent_prop = kos_mk_prop("Prop");
    kos_term* rapidmovementevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        rapidmovementevent_prop);
    kos_term* rapidmovementevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        rapidmovementevent_sigma_3);
    kos_term* rapidmovementevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        rapidmovementevent_sigma_2);
    kos_term* rapidmovementevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        rapidmovementevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "RapidMovementEvent", rapidmovementevent_sigma_0, NULL);
    // CircularTransactionsEvent: 事件类型
    // CircularTransactionsEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* circulartransactionsevent_prop = kos_mk_prop("Prop");
    kos_term* circulartransactionsevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        circulartransactionsevent_prop);
    kos_term* circulartransactionsevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        circulartransactionsevent_sigma_3);
    kos_term* circulartransactionsevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        circulartransactionsevent_sigma_2);
    kos_term* circulartransactionsevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        circulartransactionsevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "CircularTransactionsEvent", circulartransactionsevent_sigma_0, NULL);
    // StructuredTransactionsEvent: 事件类型
    // StructuredTransactionsEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* structuredtransactionsevent_prop = kos_mk_prop("Prop");
    kos_term* structuredtransactionsevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        structuredtransactionsevent_prop);
    kos_term* structuredtransactionsevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        structuredtransactionsevent_sigma_3);
    kos_term* structuredtransactionsevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        structuredtransactionsevent_sigma_2);
    kos_term* structuredtransactionsevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        structuredtransactionsevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "StructuredTransactionsEvent", structuredtransactionsevent_sigma_0, NULL);
    // HighValueWithoutPurposeEvent: 事件类型
    // HighValueWithoutPurposeEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* highvaluewithoutpurposeevent_prop = kos_mk_prop("Prop");
    kos_term* highvaluewithoutpurposeevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        highvaluewithoutpurposeevent_prop);
    kos_term* highvaluewithoutpurposeevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        highvaluewithoutpurposeevent_sigma_3);
    kos_term* highvaluewithoutpurposeevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        highvaluewithoutpurposeevent_sigma_2);
    kos_term* highvaluewithoutpurposeevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        highvaluewithoutpurposeevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "HighValueWithoutPurposeEvent", highvaluewithoutpurposeevent_sigma_0, NULL);
    // CrossBorderNoBusinessEvent: 事件类型
    // CrossBorderNoBusinessEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* crossbordernobusinessevent_prop = kos_mk_prop("Prop");
    kos_term* crossbordernobusinessevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        crossbordernobusinessevent_prop);
    kos_term* crossbordernobusinessevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        crossbordernobusinessevent_sigma_3);
    kos_term* crossbordernobusinessevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        crossbordernobusinessevent_sigma_2);
    kos_term* crossbordernobusinessevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        crossbordernobusinessevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "CrossBorderNoBusinessEvent", crossbordernobusinessevent_sigma_0, NULL);
    // MultipleAccountsSameBeneficiaryEvent: 事件类型
    // MultipleAccountsSameBeneficiaryEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* multipleaccountssamebeneficiaryevent_prop = kos_mk_prop("Prop");
    kos_term* multipleaccountssamebeneficiaryevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        multipleaccountssamebeneficiaryevent_prop);
    kos_term* multipleaccountssamebeneficiaryevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        multipleaccountssamebeneficiaryevent_sigma_3);
    kos_term* multipleaccountssamebeneficiaryevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        multipleaccountssamebeneficiaryevent_sigma_2);
    kos_term* multipleaccountssamebeneficiaryevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        multipleaccountssamebeneficiaryevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "MultipleAccountsSameBeneficiaryEvent", multipleaccountssamebeneficiaryevent_sigma_0, NULL);
    // FrequentSmallDepositsEvent: 事件类型
    // FrequentSmallDepositsEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* frequentsmalldepositsevent_prop = kos_mk_prop("Prop");
    kos_term* frequentsmalldepositsevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        frequentsmalldepositsevent_prop);
    kos_term* frequentsmalldepositsevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        frequentsmalldepositsevent_sigma_3);
    kos_term* frequentsmalldepositsevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        frequentsmalldepositsevent_sigma_2);
    kos_term* frequentsmalldepositsevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        frequentsmalldepositsevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "FrequentSmallDepositsEvent", frequentsmalldepositsevent_sigma_0, NULL);
    // LargeCashTransactionsEvent: 事件类型
    // LargeCashTransactionsEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* largecashtransactionsevent_prop = kos_mk_prop("Prop");
    kos_term* largecashtransactionsevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        largecashtransactionsevent_prop);
    kos_term* largecashtransactionsevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        largecashtransactionsevent_sigma_3);
    kos_term* largecashtransactionsevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        largecashtransactionsevent_sigma_2);
    kos_term* largecashtransactionsevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        largecashtransactionsevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "LargeCashTransactionsEvent", largecashtransactionsevent_sigma_0, NULL);
    // TransactionsWithSanctionedCountriesEvent: 事件类型
    // TransactionsWithSanctionedCountriesEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* transactionswithsanctionedcountriesevent_prop = kos_mk_prop("Prop");
    kos_term* transactionswithsanctionedcountriesevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        transactionswithsanctionedcountriesevent_prop);
    kos_term* transactionswithsanctionedcountriesevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        transactionswithsanctionedcountriesevent_sigma_3);
    kos_term* transactionswithsanctionedcountriesevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        transactionswithsanctionedcountriesevent_sigma_2);
    kos_term* transactionswithsanctionedcountriesevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        transactionswithsanctionedcountriesevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "TransactionsWithSanctionedCountriesEvent", transactionswithsanctionedcountriesevent_sigma_0, NULL);
    // TransactionsWithPEPEvent: 事件类型
    // TransactionsWithPEPEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* transactionswithpepevent_prop = kos_mk_prop("Prop");
    kos_term* transactionswithpepevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        transactionswithpepevent_prop);
    kos_term* transactionswithpepevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        transactionswithpepevent_sigma_3);
    kos_term* transactionswithpepevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        transactionswithpepevent_sigma_2);
    kos_term* transactionswithpepevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        transactionswithpepevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "TransactionsWithPEPEvent", transactionswithpepevent_sigma_0, NULL);
    // ComplexTransactionChainEvent: 事件类型
    // ComplexTransactionChainEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* complextransactionchainevent_prop = kos_mk_prop("Prop");
    kos_term* complextransactionchainevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        complextransactionchainevent_prop);
    kos_term* complextransactionchainevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        complextransactionchainevent_sigma_3);
    kos_term* complextransactionchainevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        complextransactionchainevent_sigma_2);
    kos_term* complextransactionchainevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        complextransactionchainevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "ComplexTransactionChainEvent", complextransactionchainevent_sigma_0, NULL);
    // TransactionsInconsistentWithProfileEvent: 事件类型
    // TransactionsInconsistentWithProfileEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* transactionsinconsistentwithprofileevent_prop = kos_mk_prop("Prop");
    kos_term* transactionsinconsistentwithprofileevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        transactionsinconsistentwithprofileevent_prop);
    kos_term* transactionsinconsistentwithprofileevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        transactionsinconsistentwithprofileevent_sigma_3);
    kos_term* transactionsinconsistentwithprofileevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        transactionsinconsistentwithprofileevent_sigma_2);
    kos_term* transactionsinconsistentwithprofileevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        transactionsinconsistentwithprofileevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "TransactionsInconsistentWithProfileEvent", transactionsinconsistentwithprofileevent_sigma_0, NULL);
    // RapidAccountOpeningClosingEvent: 事件类型
    // RapidAccountOpeningClosingEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* rapidaccountopeningclosingevent_prop = kos_mk_prop("Prop");
    kos_term* rapidaccountopeningclosingevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        rapidaccountopeningclosingevent_prop);
    kos_term* rapidaccountopeningclosingevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        rapidaccountopeningclosingevent_sigma_3);
    kos_term* rapidaccountopeningclosingevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        rapidaccountopeningclosingevent_sigma_2);
    kos_term* rapidaccountopeningclosingevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        rapidaccountopeningclosingevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "RapidAccountOpeningClosingEvent", rapidaccountopeningclosingevent_sigma_0, NULL);
    // UnexplainedWealthEvent: 事件类型
    // UnexplainedWealthEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* unexplainedwealthevent_prop = kos_mk_prop("Prop");
    kos_term* unexplainedwealthevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        unexplainedwealthevent_prop);
    kos_term* unexplainedwealthevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        unexplainedwealthevent_sigma_3);
    kos_term* unexplainedwealthevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        unexplainedwealthevent_sigma_2);
    kos_term* unexplainedwealthevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        unexplainedwealthevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "UnexplainedWealthEvent", unexplainedwealthevent_sigma_0, NULL);
    // RoundTripTransactionsEvent: 事件类型
    // RoundTripTransactionsEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* roundtriptransactionsevent_prop = kos_mk_prop("Prop");
    kos_term* roundtriptransactionsevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        roundtriptransactionsevent_prop);
    kos_term* roundtriptransactionsevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        roundtriptransactionsevent_sigma_3);
    kos_term* roundtriptransactionsevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        roundtriptransactionsevent_sigma_2);
    kos_term* roundtriptransactionsevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        roundtriptransactionsevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "RoundTripTransactionsEvent", roundtriptransactionsevent_sigma_0, NULL);
    // BackToBackLoansEvent: 事件类型
    // BackToBackLoansEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* backtobackloansevent_prop = kos_mk_prop("Prop");
    kos_term* backtobackloansevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        backtobackloansevent_prop);
    kos_term* backtobackloansevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        backtobackloansevent_sigma_3);
    kos_term* backtobackloansevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        backtobackloansevent_sigma_2);
    kos_term* backtobackloansevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        backtobackloansevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "BackToBackLoansEvent", backtobackloansevent_sigma_0, NULL);
    // FalseIdentityEvent: 事件类型
    // FalseIdentityEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* falseidentityevent_prop = kos_mk_prop("Prop");
    kos_term* falseidentityevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        falseidentityevent_prop);
    kos_term* falseidentityevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        falseidentityevent_sigma_3);
    kos_term* falseidentityevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        falseidentityevent_sigma_2);
    kos_term* falseidentityevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        falseidentityevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "FalseIdentityEvent", falseidentityevent_sigma_0, NULL);
    // IdentityTheftEvent: 事件类型
    // IdentityTheftEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* identitytheftevent_prop = kos_mk_prop("Prop");
    kos_term* identitytheftevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        identitytheftevent_prop);
    kos_term* identitytheftevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        identitytheftevent_sigma_3);
    kos_term* identitytheftevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        identitytheftevent_sigma_2);
    kos_term* identitytheftevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        identitytheftevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "IdentityTheftEvent", identitytheftevent_sigma_0, NULL);
    // AccountTakeoverEvent: 事件类型
    // AccountTakeoverEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* accounttakeoverevent_prop = kos_mk_prop("Prop");
    kos_term* accounttakeoverevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        accounttakeoverevent_prop);
    kos_term* accounttakeoverevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        accounttakeoverevent_sigma_3);
    kos_term* accounttakeoverevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        accounttakeoverevent_sigma_2);
    kos_term* accounttakeoverevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        accounttakeoverevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "AccountTakeoverEvent", accounttakeoverevent_sigma_0, NULL);
    // SyntheticIdentityEvent: 事件类型
    // SyntheticIdentityEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* syntheticidentityevent_prop = kos_mk_prop("Prop");
    kos_term* syntheticidentityevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        syntheticidentityevent_prop);
    kos_term* syntheticidentityevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        syntheticidentityevent_sigma_3);
    kos_term* syntheticidentityevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        syntheticidentityevent_sigma_2);
    kos_term* syntheticidentityevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        syntheticidentityevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "SyntheticIdentityEvent", syntheticidentityevent_sigma_0, NULL);
    // MuleAccountEvent: 事件类型
    // MuleAccountEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* muleaccountevent_prop = kos_mk_prop("Prop");
    kos_term* muleaccountevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        muleaccountevent_prop);
    kos_term* muleaccountevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        muleaccountevent_sigma_3);
    kos_term* muleaccountevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        muleaccountevent_sigma_2);
    kos_term* muleaccountevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        muleaccountevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "MuleAccountEvent", muleaccountevent_sigma_0, NULL);
    // FunnelAccountEvent: 事件类型
    // FunnelAccountEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* funnelaccountevent_prop = kos_mk_prop("Prop");
    kos_term* funnelaccountevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        funnelaccountevent_prop);
    kos_term* funnelaccountevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        funnelaccountevent_sigma_3);
    kos_term* funnelaccountevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        funnelaccountevent_sigma_2);
    kos_term* funnelaccountevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        funnelaccountevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "FunnelAccountEvent", funnelaccountevent_sigma_0, NULL);
    // SmurfingPatternEvent: 事件类型
    // SmurfingPatternEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* smurfingpatternevent_prop = kos_mk_prop("Prop");
    kos_term* smurfingpatternevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        smurfingpatternevent_prop);
    kos_term* smurfingpatternevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        smurfingpatternevent_sigma_3);
    kos_term* smurfingpatternevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        smurfingpatternevent_sigma_2);
    kos_term* smurfingpatternevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        smurfingpatternevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "SmurfingPatternEvent", smurfingpatternevent_sigma_0, NULL);
    // StructuringPatternEvent: 事件类型
    // StructuringPatternEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* structuringpatternevent_prop = kos_mk_prop("Prop");
    kos_term* structuringpatternevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        structuringpatternevent_prop);
    kos_term* structuringpatternevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        structuringpatternevent_sigma_3);
    kos_term* structuringpatternevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        structuringpatternevent_sigma_2);
    kos_term* structuringpatternevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        structuringpatternevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "StructuringPatternEvent", structuringpatternevent_sigma_0, NULL);
    // LayeringPatternEvent: 事件类型
    // LayeringPatternEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* layeringpatternevent_prop = kos_mk_prop("Prop");
    kos_term* layeringpatternevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        layeringpatternevent_prop);
    kos_term* layeringpatternevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        layeringpatternevent_sigma_3);
    kos_term* layeringpatternevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        layeringpatternevent_sigma_2);
    kos_term* layeringpatternevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        layeringpatternevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "LayeringPatternEvent", layeringpatternevent_sigma_0, NULL);
    // IntegrationPatternEvent: 事件类型
    // IntegrationPatternEvent = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(description: Prop) Σ(timestamp: Time) Prop
    kos_term* integrationpatternevent_prop = kos_mk_prop("Prop");
    kos_term* integrationpatternevent_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        integrationpatternevent_prop);
    kos_term* integrationpatternevent_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        integrationpatternevent_sigma_3);
    kos_term* integrationpatternevent_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        integrationpatternevent_sigma_2);
    kos_term* integrationpatternevent_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        integrationpatternevent_sigma_1);
    kos_ontology_add_type_definition(ontology, "IntegrationPatternEvent", integrationpatternevent_sigma_0, NULL);
    // StructuringSuspicion: 事件类型
    // StructuringSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* structuringsuspicion_prop = kos_mk_prop("Prop");
    kos_term* structuringsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        structuringsuspicion_prop);
    kos_term* structuringsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        structuringsuspicion_sigma_4);
    kos_term* structuringsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        structuringsuspicion_sigma_3);
    kos_term* structuringsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        structuringsuspicion_sigma_2);
    kos_term* structuringsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        structuringsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "StructuringSuspicion", structuringsuspicion_sigma_0, NULL);
    // SmurfingSuspicion: 事件类型
    // SmurfingSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* smurfingsuspicion_prop = kos_mk_prop("Prop");
    kos_term* smurfingsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        smurfingsuspicion_prop);
    kos_term* smurfingsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        smurfingsuspicion_sigma_4);
    kos_term* smurfingsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        smurfingsuspicion_sigma_3);
    kos_term* smurfingsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        smurfingsuspicion_sigma_2);
    kos_term* smurfingsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        smurfingsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "SmurfingSuspicion", smurfingsuspicion_sigma_0, NULL);
    // LayeringSuspicion: 事件类型
    // LayeringSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* layeringsuspicion_prop = kos_mk_prop("Prop");
    kos_term* layeringsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        layeringsuspicion_prop);
    kos_term* layeringsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        layeringsuspicion_sigma_4);
    kos_term* layeringsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        layeringsuspicion_sigma_3);
    kos_term* layeringsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        layeringsuspicion_sigma_2);
    kos_term* layeringsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        layeringsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "LayeringSuspicion", layeringsuspicion_sigma_0, NULL);
    // IntegrationSuspicion: 事件类型
    // IntegrationSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* integrationsuspicion_prop = kos_mk_prop("Prop");
    kos_term* integrationsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        integrationsuspicion_prop);
    kos_term* integrationsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        integrationsuspicion_sigma_4);
    kos_term* integrationsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        integrationsuspicion_sigma_3);
    kos_term* integrationsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        integrationsuspicion_sigma_2);
    kos_term* integrationsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        integrationsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "IntegrationSuspicion", integrationsuspicion_sigma_0, NULL);
    // TradeBasedSuspicion: 事件类型
    // TradeBasedSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* tradebasedsuspicion_prop = kos_mk_prop("Prop");
    kos_term* tradebasedsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        tradebasedsuspicion_prop);
    kos_term* tradebasedsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        tradebasedsuspicion_sigma_4);
    kos_term* tradebasedsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        tradebasedsuspicion_sigma_3);
    kos_term* tradebasedsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        tradebasedsuspicion_sigma_2);
    kos_term* tradebasedsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        tradebasedsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "TradeBasedSuspicion", tradebasedsuspicion_sigma_0, NULL);
    // ShellCompanySuspicion: 事件类型
    // ShellCompanySuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* shellcompanysuspicion_prop = kos_mk_prop("Prop");
    kos_term* shellcompanysuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        shellcompanysuspicion_prop);
    kos_term* shellcompanysuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        shellcompanysuspicion_sigma_4);
    kos_term* shellcompanysuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        shellcompanysuspicion_sigma_3);
    kos_term* shellcompanysuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        shellcompanysuspicion_sigma_2);
    kos_term* shellcompanysuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        shellcompanysuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "ShellCompanySuspicion", shellcompanysuspicion_sigma_0, NULL);
    // NomineeAccountSuspicion: 事件类型
    // NomineeAccountSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* nomineeaccountsuspicion_prop = kos_mk_prop("Prop");
    kos_term* nomineeaccountsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        nomineeaccountsuspicion_prop);
    kos_term* nomineeaccountsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        nomineeaccountsuspicion_sigma_4);
    kos_term* nomineeaccountsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        nomineeaccountsuspicion_sigma_3);
    kos_term* nomineeaccountsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        nomineeaccountsuspicion_sigma_2);
    kos_term* nomineeaccountsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        nomineeaccountsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "NomineeAccountSuspicion", nomineeaccountsuspicion_sigma_0, NULL);
    // OffshoreStructureSuspicion: 事件类型
    // OffshoreStructureSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* offshorestructuresuspicion_prop = kos_mk_prop("Prop");
    kos_term* offshorestructuresuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        offshorestructuresuspicion_prop);
    kos_term* offshorestructuresuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        offshorestructuresuspicion_sigma_4);
    kos_term* offshorestructuresuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        offshorestructuresuspicion_sigma_3);
    kos_term* offshorestructuresuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        offshorestructuresuspicion_sigma_2);
    kos_term* offshorestructuresuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        offshorestructuresuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "OffshoreStructureSuspicion", offshorestructuresuspicion_sigma_0, NULL);
    // CryptocurrencyMixerSuspicion: 事件类型
    // CryptocurrencyMixerSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* cryptocurrencymixersuspicion_prop = kos_mk_prop("Prop");
    kos_term* cryptocurrencymixersuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        cryptocurrencymixersuspicion_prop);
    kos_term* cryptocurrencymixersuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        cryptocurrencymixersuspicion_sigma_4);
    kos_term* cryptocurrencymixersuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        cryptocurrencymixersuspicion_sigma_3);
    kos_term* cryptocurrencymixersuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        cryptocurrencymixersuspicion_sigma_2);
    kos_term* cryptocurrencymixersuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        cryptocurrencymixersuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "CryptocurrencyMixerSuspicion", cryptocurrencymixersuspicion_sigma_0, NULL);
    // HawalaSuspicion: 事件类型
    // HawalaSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* hawalasuspicion_prop = kos_mk_prop("Prop");
    kos_term* hawalasuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        hawalasuspicion_prop);
    kos_term* hawalasuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        hawalasuspicion_sigma_4);
    kos_term* hawalasuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        hawalasuspicion_sigma_3);
    kos_term* hawalasuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        hawalasuspicion_sigma_2);
    kos_term* hawalasuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        hawalasuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "HawalaSuspicion", hawalasuspicion_sigma_0, NULL);
    // UndergroundBankingSuspicion: 事件类型
    // UndergroundBankingSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* undergroundbankingsuspicion_prop = kos_mk_prop("Prop");
    kos_term* undergroundbankingsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        undergroundbankingsuspicion_prop);
    kos_term* undergroundbankingsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        undergroundbankingsuspicion_sigma_4);
    kos_term* undergroundbankingsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        undergroundbankingsuspicion_sigma_3);
    kos_term* undergroundbankingsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        undergroundbankingsuspicion_sigma_2);
    kos_term* undergroundbankingsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        undergroundbankingsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "UndergroundBankingSuspicion", undergroundbankingsuspicion_sigma_0, NULL);
    // CasinoLaunderingSuspicion: 事件类型
    // CasinoLaunderingSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* casinolaunderingsuspicion_prop = kos_mk_prop("Prop");
    kos_term* casinolaunderingsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        casinolaunderingsuspicion_prop);
    kos_term* casinolaunderingsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        casinolaunderingsuspicion_sigma_4);
    kos_term* casinolaunderingsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        casinolaunderingsuspicion_sigma_3);
    kos_term* casinolaunderingsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        casinolaunderingsuspicion_sigma_2);
    kos_term* casinolaunderingsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        casinolaunderingsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "CasinoLaunderingSuspicion", casinolaunderingsuspicion_sigma_0, NULL);
    // RealEstateLaunderingSuspicion: 事件类型
    // RealEstateLaunderingSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* realestatelaunderingsuspicion_prop = kos_mk_prop("Prop");
    kos_term* realestatelaunderingsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        realestatelaunderingsuspicion_prop);
    kos_term* realestatelaunderingsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        realestatelaunderingsuspicion_sigma_4);
    kos_term* realestatelaunderingsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        realestatelaunderingsuspicion_sigma_3);
    kos_term* realestatelaunderingsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        realestatelaunderingsuspicion_sigma_2);
    kos_term* realestatelaunderingsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        realestatelaunderingsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "RealEstateLaunderingSuspicion", realestatelaunderingsuspicion_sigma_0, NULL);
    // ArtLaunderingSuspicion: 事件类型
    // ArtLaunderingSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* artlaunderingsuspicion_prop = kos_mk_prop("Prop");
    kos_term* artlaunderingsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        artlaunderingsuspicion_prop);
    kos_term* artlaunderingsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        artlaunderingsuspicion_sigma_4);
    kos_term* artlaunderingsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        artlaunderingsuspicion_sigma_3);
    kos_term* artlaunderingsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        artlaunderingsuspicion_sigma_2);
    kos_term* artlaunderingsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        artlaunderingsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "ArtLaunderingSuspicion", artlaunderingsuspicion_sigma_0, NULL);
    // JewelryLaunderingSuspicion: 事件类型
    // JewelryLaunderingSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* jewelrylaunderingsuspicion_prop = kos_mk_prop("Prop");
    kos_term* jewelrylaunderingsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        jewelrylaunderingsuspicion_prop);
    kos_term* jewelrylaunderingsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        jewelrylaunderingsuspicion_sigma_4);
    kos_term* jewelrylaunderingsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        jewelrylaunderingsuspicion_sigma_3);
    kos_term* jewelrylaunderingsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        jewelrylaunderingsuspicion_sigma_2);
    kos_term* jewelrylaunderingsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        jewelrylaunderingsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "JewelryLaunderingSuspicion", jewelrylaunderingsuspicion_sigma_0, NULL);
    // VirtualAssetLaunderingSuspicion: 事件类型
    // VirtualAssetLaunderingSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* virtualassetlaunderingsuspicion_prop = kos_mk_prop("Prop");
    kos_term* virtualassetlaunderingsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        virtualassetlaunderingsuspicion_prop);
    kos_term* virtualassetlaunderingsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        virtualassetlaunderingsuspicion_sigma_4);
    kos_term* virtualassetlaunderingsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        virtualassetlaunderingsuspicion_sigma_3);
    kos_term* virtualassetlaunderingsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        virtualassetlaunderingsuspicion_sigma_2);
    kos_term* virtualassetlaunderingsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        virtualassetlaunderingsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "VirtualAssetLaunderingSuspicion", virtualassetlaunderingsuspicion_sigma_0, NULL);
    // TradeBasedLaunderingSuspicion: 事件类型
    // TradeBasedLaunderingSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* tradebasedlaunderingsuspicion_prop = kos_mk_prop("Prop");
    kos_term* tradebasedlaunderingsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        tradebasedlaunderingsuspicion_prop);
    kos_term* tradebasedlaunderingsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        tradebasedlaunderingsuspicion_sigma_4);
    kos_term* tradebasedlaunderingsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        tradebasedlaunderingsuspicion_sigma_3);
    kos_term* tradebasedlaunderingsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        tradebasedlaunderingsuspicion_sigma_2);
    kos_term* tradebasedlaunderingsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        tradebasedlaunderingsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "TradeBasedLaunderingSuspicion", tradebasedlaunderingsuspicion_sigma_0, NULL);
    // InvoiceFraudSuspicion: 事件类型
    // InvoiceFraudSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* invoicefraudsuspicion_prop = kos_mk_prop("Prop");
    kos_term* invoicefraudsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        invoicefraudsuspicion_prop);
    kos_term* invoicefraudsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        invoicefraudsuspicion_sigma_4);
    kos_term* invoicefraudsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        invoicefraudsuspicion_sigma_3);
    kos_term* invoicefraudsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        invoicefraudsuspicion_sigma_2);
    kos_term* invoicefraudsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        invoicefraudsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "InvoiceFraudSuspicion", invoicefraudsuspicion_sigma_0, NULL);
    // OverInvoicingSuspicion: 事件类型
    // OverInvoicingSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* overinvoicingsuspicion_prop = kos_mk_prop("Prop");
    kos_term* overinvoicingsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        overinvoicingsuspicion_prop);
    kos_term* overinvoicingsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        overinvoicingsuspicion_sigma_4);
    kos_term* overinvoicingsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        overinvoicingsuspicion_sigma_3);
    kos_term* overinvoicingsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        overinvoicingsuspicion_sigma_2);
    kos_term* overinvoicingsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        overinvoicingsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "OverInvoicingSuspicion", overinvoicingsuspicion_sigma_0, NULL);
    // UnderInvoicingSuspicion: 事件类型
    // UnderInvoicingSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* underinvoicingsuspicion_prop = kos_mk_prop("Prop");
    kos_term* underinvoicingsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        underinvoicingsuspicion_prop);
    kos_term* underinvoicingsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        underinvoicingsuspicion_sigma_4);
    kos_term* underinvoicingsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        underinvoicingsuspicion_sigma_3);
    kos_term* underinvoicingsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        underinvoicingsuspicion_sigma_2);
    kos_term* underinvoicingsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        underinvoicingsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "UnderInvoicingSuspicion", underinvoicingsuspicion_sigma_0, NULL);
    // FalseInvoicingSuspicion: 事件类型
    // FalseInvoicingSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* falseinvoicingsuspicion_prop = kos_mk_prop("Prop");
    kos_term* falseinvoicingsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        falseinvoicingsuspicion_prop);
    kos_term* falseinvoicingsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        falseinvoicingsuspicion_sigma_4);
    kos_term* falseinvoicingsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        falseinvoicingsuspicion_sigma_3);
    kos_term* falseinvoicingsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        falseinvoicingsuspicion_sigma_2);
    kos_term* falseinvoicingsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        falseinvoicingsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "FalseInvoicingSuspicion", falseinvoicingsuspicion_sigma_0, NULL);
    // RoundTrippingSuspicion: 事件类型
    // RoundTrippingSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* roundtrippingsuspicion_prop = kos_mk_prop("Prop");
    kos_term* roundtrippingsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        roundtrippingsuspicion_prop);
    kos_term* roundtrippingsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        roundtrippingsuspicion_sigma_4);
    kos_term* roundtrippingsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        roundtrippingsuspicion_sigma_3);
    kos_term* roundtrippingsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        roundtrippingsuspicion_sigma_2);
    kos_term* roundtrippingsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        roundtrippingsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "RoundTrippingSuspicion", roundtrippingsuspicion_sigma_0, NULL);
    // LoanBackSuspicion: 事件类型
    // LoanBackSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* loanbacksuspicion_prop = kos_mk_prop("Prop");
    kos_term* loanbacksuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        loanbacksuspicion_prop);
    kos_term* loanbacksuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        loanbacksuspicion_sigma_4);
    kos_term* loanbacksuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        loanbacksuspicion_sigma_3);
    kos_term* loanbacksuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        loanbacksuspicion_sigma_2);
    kos_term* loanbacksuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        loanbacksuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "LoanBackSuspicion", loanbacksuspicion_sigma_0, NULL);
    // GiftLaunderingSuspicion: 事件类型
    // GiftLaunderingSuspicion = Σ(case_id: TransactionID) Σ(account_id: AccountID) Σ(evidence: Prop) Σ(risk_level: Prop) Σ(timestamp: Time) Prop
    kos_term* giftlaunderingsuspicion_prop = kos_mk_prop("Prop");
    kos_term* giftlaunderingsuspicion_sigma_4 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Time"),
        giftlaunderingsuspicion_prop);
    kos_term* giftlaunderingsuspicion_sigma_3 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        giftlaunderingsuspicion_sigma_4);
    kos_term* giftlaunderingsuspicion_sigma_2 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "Prop"),
        giftlaunderingsuspicion_sigma_3);
    kos_term* giftlaunderingsuspicion_sigma_1 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "AccountID"),
        giftlaunderingsuspicion_sigma_2);
    kos_term* giftlaunderingsuspicion_sigma_0 = kos_mk_sigma(
        kos_ontology_find_type_definition(ontology, "TransactionID"),
        giftlaunderingsuspicion_sigma_1);
    kos_ontology_add_type_definition(ontology, "GiftLaunderingSuspicion", giftlaunderingsuspicion_sigma_0, NULL);

    // ========== 账户类型组合 ==========
    // SavingsAccountInUnitedStates
    kos_term* savingsaccountinunitedstates_type = kos_mk_prop("SavingsAccountInUnitedStates");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInUnitedStates", savingsaccountinunitedstates_type, NULL);
    // SavingsAccountInUnitedKingdom
    kos_term* savingsaccountinunitedkingdom_type = kos_mk_prop("SavingsAccountInUnitedKingdom");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInUnitedKingdom", savingsaccountinunitedkingdom_type, NULL);
    // SavingsAccountInSwitzerland
    kos_term* savingsaccountinswitzerland_type = kos_mk_prop("SavingsAccountInSwitzerland");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInSwitzerland", savingsaccountinswitzerland_type, NULL);
    // SavingsAccountInSingapore
    kos_term* savingsaccountinsingapore_type = kos_mk_prop("SavingsAccountInSingapore");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInSingapore", savingsaccountinsingapore_type, NULL);
    // SavingsAccountInHongKong
    kos_term* savingsaccountinhongkong_type = kos_mk_prop("SavingsAccountInHongKong");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInHongKong", savingsaccountinhongkong_type, NULL);
    // SavingsAccountInLuxembourg
    kos_term* savingsaccountinluxembourg_type = kos_mk_prop("SavingsAccountInLuxembourg");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInLuxembourg", savingsaccountinluxembourg_type, NULL);
    // SavingsAccountInCaymanIslands
    kos_term* savingsaccountincaymanislands_type = kos_mk_prop("SavingsAccountInCaymanIslands");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInCaymanIslands", savingsaccountincaymanislands_type, NULL);
    // SavingsAccountInBritishVirginIslands
    kos_term* savingsaccountinbritishvirginislands_type = kos_mk_prop("SavingsAccountInBritishVirginIslands");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInBritishVirginIslands", savingsaccountinbritishvirginislands_type, NULL);
    // SavingsAccountInBermuda
    kos_term* savingsaccountinbermuda_type = kos_mk_prop("SavingsAccountInBermuda");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInBermuda", savingsaccountinbermuda_type, NULL);
    // SavingsAccountInPanama
    kos_term* savingsaccountinpanama_type = kos_mk_prop("SavingsAccountInPanama");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInPanama", savingsaccountinpanama_type, NULL);
    // SavingsAccountInBahamas
    kos_term* savingsaccountinbahamas_type = kos_mk_prop("SavingsAccountInBahamas");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInBahamas", savingsaccountinbahamas_type, NULL);
    // SavingsAccountInJersey
    kos_term* savingsaccountinjersey_type = kos_mk_prop("SavingsAccountInJersey");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInJersey", savingsaccountinjersey_type, NULL);
    // SavingsAccountInGuernsey
    kos_term* savingsaccountinguernsey_type = kos_mk_prop("SavingsAccountInGuernsey");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInGuernsey", savingsaccountinguernsey_type, NULL);
    // SavingsAccountInIsleOfMan
    kos_term* savingsaccountinisleofman_type = kos_mk_prop("SavingsAccountInIsleOfMan");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInIsleOfMan", savingsaccountinisleofman_type, NULL);
    // SavingsAccountInCyprus
    kos_term* savingsaccountincyprus_type = kos_mk_prop("SavingsAccountInCyprus");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInCyprus", savingsaccountincyprus_type, NULL);
    // SavingsAccountInMalta
    kos_term* savingsaccountinmalta_type = kos_mk_prop("SavingsAccountInMalta");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInMalta", savingsaccountinmalta_type, NULL);
    // SavingsAccountInIreland
    kos_term* savingsaccountinireland_type = kos_mk_prop("SavingsAccountInIreland");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInIreland", savingsaccountinireland_type, NULL);
    // SavingsAccountInNetherlands
    kos_term* savingsaccountinnetherlands_type = kos_mk_prop("SavingsAccountInNetherlands");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInNetherlands", savingsaccountinnetherlands_type, NULL);
    // SavingsAccountInBelgium
    kos_term* savingsaccountinbelgium_type = kos_mk_prop("SavingsAccountInBelgium");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInBelgium", savingsaccountinbelgium_type, NULL);
    // SavingsAccountInGermany
    kos_term* savingsaccountingermany_type = kos_mk_prop("SavingsAccountInGermany");
    kos_ontology_add_type_definition(ontology, "SavingsAccountInGermany", savingsaccountingermany_type, NULL);
    // CheckingAccountInUnitedStates
    kos_term* checkingaccountinunitedstates_type = kos_mk_prop("CheckingAccountInUnitedStates");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInUnitedStates", checkingaccountinunitedstates_type, NULL);
    // CheckingAccountInUnitedKingdom
    kos_term* checkingaccountinunitedkingdom_type = kos_mk_prop("CheckingAccountInUnitedKingdom");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInUnitedKingdom", checkingaccountinunitedkingdom_type, NULL);
    // CheckingAccountInSwitzerland
    kos_term* checkingaccountinswitzerland_type = kos_mk_prop("CheckingAccountInSwitzerland");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInSwitzerland", checkingaccountinswitzerland_type, NULL);
    // CheckingAccountInSingapore
    kos_term* checkingaccountinsingapore_type = kos_mk_prop("CheckingAccountInSingapore");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInSingapore", checkingaccountinsingapore_type, NULL);
    // CheckingAccountInHongKong
    kos_term* checkingaccountinhongkong_type = kos_mk_prop("CheckingAccountInHongKong");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInHongKong", checkingaccountinhongkong_type, NULL);
    // CheckingAccountInLuxembourg
    kos_term* checkingaccountinluxembourg_type = kos_mk_prop("CheckingAccountInLuxembourg");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInLuxembourg", checkingaccountinluxembourg_type, NULL);
    // CheckingAccountInCaymanIslands
    kos_term* checkingaccountincaymanislands_type = kos_mk_prop("CheckingAccountInCaymanIslands");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInCaymanIslands", checkingaccountincaymanislands_type, NULL);
    // CheckingAccountInBritishVirginIslands
    kos_term* checkingaccountinbritishvirginislands_type = kos_mk_prop("CheckingAccountInBritishVirginIslands");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInBritishVirginIslands", checkingaccountinbritishvirginislands_type, NULL);
    // CheckingAccountInBermuda
    kos_term* checkingaccountinbermuda_type = kos_mk_prop("CheckingAccountInBermuda");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInBermuda", checkingaccountinbermuda_type, NULL);
    // CheckingAccountInPanama
    kos_term* checkingaccountinpanama_type = kos_mk_prop("CheckingAccountInPanama");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInPanama", checkingaccountinpanama_type, NULL);
    // CheckingAccountInBahamas
    kos_term* checkingaccountinbahamas_type = kos_mk_prop("CheckingAccountInBahamas");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInBahamas", checkingaccountinbahamas_type, NULL);
    // CheckingAccountInJersey
    kos_term* checkingaccountinjersey_type = kos_mk_prop("CheckingAccountInJersey");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInJersey", checkingaccountinjersey_type, NULL);
    // CheckingAccountInGuernsey
    kos_term* checkingaccountinguernsey_type = kos_mk_prop("CheckingAccountInGuernsey");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInGuernsey", checkingaccountinguernsey_type, NULL);
    // CheckingAccountInIsleOfMan
    kos_term* checkingaccountinisleofman_type = kos_mk_prop("CheckingAccountInIsleOfMan");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInIsleOfMan", checkingaccountinisleofman_type, NULL);
    // CheckingAccountInCyprus
    kos_term* checkingaccountincyprus_type = kos_mk_prop("CheckingAccountInCyprus");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInCyprus", checkingaccountincyprus_type, NULL);
    // CheckingAccountInMalta
    kos_term* checkingaccountinmalta_type = kos_mk_prop("CheckingAccountInMalta");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInMalta", checkingaccountinmalta_type, NULL);
    // CheckingAccountInIreland
    kos_term* checkingaccountinireland_type = kos_mk_prop("CheckingAccountInIreland");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInIreland", checkingaccountinireland_type, NULL);
    // CheckingAccountInNetherlands
    kos_term* checkingaccountinnetherlands_type = kos_mk_prop("CheckingAccountInNetherlands");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInNetherlands", checkingaccountinnetherlands_type, NULL);
    // CheckingAccountInBelgium
    kos_term* checkingaccountinbelgium_type = kos_mk_prop("CheckingAccountInBelgium");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInBelgium", checkingaccountinbelgium_type, NULL);
    // CheckingAccountInGermany
    kos_term* checkingaccountingermany_type = kos_mk_prop("CheckingAccountInGermany");
    kos_ontology_add_type_definition(ontology, "CheckingAccountInGermany", checkingaccountingermany_type, NULL);
    // CurrentAccountInUnitedStates
    kos_term* currentaccountinunitedstates_type = kos_mk_prop("CurrentAccountInUnitedStates");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInUnitedStates", currentaccountinunitedstates_type, NULL);
    // CurrentAccountInUnitedKingdom
    kos_term* currentaccountinunitedkingdom_type = kos_mk_prop("CurrentAccountInUnitedKingdom");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInUnitedKingdom", currentaccountinunitedkingdom_type, NULL);
    // CurrentAccountInSwitzerland
    kos_term* currentaccountinswitzerland_type = kos_mk_prop("CurrentAccountInSwitzerland");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInSwitzerland", currentaccountinswitzerland_type, NULL);
    // CurrentAccountInSingapore
    kos_term* currentaccountinsingapore_type = kos_mk_prop("CurrentAccountInSingapore");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInSingapore", currentaccountinsingapore_type, NULL);
    // CurrentAccountInHongKong
    kos_term* currentaccountinhongkong_type = kos_mk_prop("CurrentAccountInHongKong");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInHongKong", currentaccountinhongkong_type, NULL);
    // CurrentAccountInLuxembourg
    kos_term* currentaccountinluxembourg_type = kos_mk_prop("CurrentAccountInLuxembourg");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInLuxembourg", currentaccountinluxembourg_type, NULL);
    // CurrentAccountInCaymanIslands
    kos_term* currentaccountincaymanislands_type = kos_mk_prop("CurrentAccountInCaymanIslands");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInCaymanIslands", currentaccountincaymanislands_type, NULL);
    // CurrentAccountInBritishVirginIslands
    kos_term* currentaccountinbritishvirginislands_type = kos_mk_prop("CurrentAccountInBritishVirginIslands");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInBritishVirginIslands", currentaccountinbritishvirginislands_type, NULL);
    // CurrentAccountInBermuda
    kos_term* currentaccountinbermuda_type = kos_mk_prop("CurrentAccountInBermuda");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInBermuda", currentaccountinbermuda_type, NULL);
    // CurrentAccountInPanama
    kos_term* currentaccountinpanama_type = kos_mk_prop("CurrentAccountInPanama");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInPanama", currentaccountinpanama_type, NULL);
    // CurrentAccountInBahamas
    kos_term* currentaccountinbahamas_type = kos_mk_prop("CurrentAccountInBahamas");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInBahamas", currentaccountinbahamas_type, NULL);
    // CurrentAccountInJersey
    kos_term* currentaccountinjersey_type = kos_mk_prop("CurrentAccountInJersey");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInJersey", currentaccountinjersey_type, NULL);
    // CurrentAccountInGuernsey
    kos_term* currentaccountinguernsey_type = kos_mk_prop("CurrentAccountInGuernsey");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInGuernsey", currentaccountinguernsey_type, NULL);
    // CurrentAccountInIsleOfMan
    kos_term* currentaccountinisleofman_type = kos_mk_prop("CurrentAccountInIsleOfMan");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInIsleOfMan", currentaccountinisleofman_type, NULL);
    // CurrentAccountInCyprus
    kos_term* currentaccountincyprus_type = kos_mk_prop("CurrentAccountInCyprus");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInCyprus", currentaccountincyprus_type, NULL);
    // CurrentAccountInMalta
    kos_term* currentaccountinmalta_type = kos_mk_prop("CurrentAccountInMalta");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInMalta", currentaccountinmalta_type, NULL);
    // CurrentAccountInIreland
    kos_term* currentaccountinireland_type = kos_mk_prop("CurrentAccountInIreland");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInIreland", currentaccountinireland_type, NULL);
    // CurrentAccountInNetherlands
    kos_term* currentaccountinnetherlands_type = kos_mk_prop("CurrentAccountInNetherlands");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInNetherlands", currentaccountinnetherlands_type, NULL);
    // CurrentAccountInBelgium
    kos_term* currentaccountinbelgium_type = kos_mk_prop("CurrentAccountInBelgium");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInBelgium", currentaccountinbelgium_type, NULL);
    // CurrentAccountInGermany
    kos_term* currentaccountingermany_type = kos_mk_prop("CurrentAccountInGermany");
    kos_ontology_add_type_definition(ontology, "CurrentAccountInGermany", currentaccountingermany_type, NULL);
    // DepositAccountInUnitedStates
    kos_term* depositaccountinunitedstates_type = kos_mk_prop("DepositAccountInUnitedStates");
    kos_ontology_add_type_definition(ontology, "DepositAccountInUnitedStates", depositaccountinunitedstates_type, NULL);
    // DepositAccountInUnitedKingdom
    kos_term* depositaccountinunitedkingdom_type = kos_mk_prop("DepositAccountInUnitedKingdom");
    kos_ontology_add_type_definition(ontology, "DepositAccountInUnitedKingdom", depositaccountinunitedkingdom_type, NULL);
    // DepositAccountInSwitzerland
    kos_term* depositaccountinswitzerland_type = kos_mk_prop("DepositAccountInSwitzerland");
    kos_ontology_add_type_definition(ontology, "DepositAccountInSwitzerland", depositaccountinswitzerland_type, NULL);
    // DepositAccountInSingapore
    kos_term* depositaccountinsingapore_type = kos_mk_prop("DepositAccountInSingapore");
    kos_ontology_add_type_definition(ontology, "DepositAccountInSingapore", depositaccountinsingapore_type, NULL);
    // DepositAccountInHongKong
    kos_term* depositaccountinhongkong_type = kos_mk_prop("DepositAccountInHongKong");
    kos_ontology_add_type_definition(ontology, "DepositAccountInHongKong", depositaccountinhongkong_type, NULL);
    // DepositAccountInLuxembourg
    kos_term* depositaccountinluxembourg_type = kos_mk_prop("DepositAccountInLuxembourg");
    kos_ontology_add_type_definition(ontology, "DepositAccountInLuxembourg", depositaccountinluxembourg_type, NULL);
    // DepositAccountInCaymanIslands
    kos_term* depositaccountincaymanislands_type = kos_mk_prop("DepositAccountInCaymanIslands");
    kos_ontology_add_type_definition(ontology, "DepositAccountInCaymanIslands", depositaccountincaymanislands_type, NULL);
    // DepositAccountInBritishVirginIslands
    kos_term* depositaccountinbritishvirginislands_type = kos_mk_prop("DepositAccountInBritishVirginIslands");
    kos_ontology_add_type_definition(ontology, "DepositAccountInBritishVirginIslands", depositaccountinbritishvirginislands_type, NULL);
    // DepositAccountInBermuda
    kos_term* depositaccountinbermuda_type = kos_mk_prop("DepositAccountInBermuda");
    kos_ontology_add_type_definition(ontology, "DepositAccountInBermuda", depositaccountinbermuda_type, NULL);
    // DepositAccountInPanama
    kos_term* depositaccountinpanama_type = kos_mk_prop("DepositAccountInPanama");
    kos_ontology_add_type_definition(ontology, "DepositAccountInPanama", depositaccountinpanama_type, NULL);
    // DepositAccountInBahamas
    kos_term* depositaccountinbahamas_type = kos_mk_prop("DepositAccountInBahamas");
    kos_ontology_add_type_definition(ontology, "DepositAccountInBahamas", depositaccountinbahamas_type, NULL);
    // DepositAccountInJersey
    kos_term* depositaccountinjersey_type = kos_mk_prop("DepositAccountInJersey");
    kos_ontology_add_type_definition(ontology, "DepositAccountInJersey", depositaccountinjersey_type, NULL);
    // DepositAccountInGuernsey
    kos_term* depositaccountinguernsey_type = kos_mk_prop("DepositAccountInGuernsey");
    kos_ontology_add_type_definition(ontology, "DepositAccountInGuernsey", depositaccountinguernsey_type, NULL);
    // DepositAccountInIsleOfMan
    kos_term* depositaccountinisleofman_type = kos_mk_prop("DepositAccountInIsleOfMan");
    kos_ontology_add_type_definition(ontology, "DepositAccountInIsleOfMan", depositaccountinisleofman_type, NULL);
    // DepositAccountInCyprus
    kos_term* depositaccountincyprus_type = kos_mk_prop("DepositAccountInCyprus");
    kos_ontology_add_type_definition(ontology, "DepositAccountInCyprus", depositaccountincyprus_type, NULL);
    // DepositAccountInMalta
    kos_term* depositaccountinmalta_type = kos_mk_prop("DepositAccountInMalta");
    kos_ontology_add_type_definition(ontology, "DepositAccountInMalta", depositaccountinmalta_type, NULL);
    // DepositAccountInIreland
    kos_term* depositaccountinireland_type = kos_mk_prop("DepositAccountInIreland");
    kos_ontology_add_type_definition(ontology, "DepositAccountInIreland", depositaccountinireland_type, NULL);
    // DepositAccountInNetherlands
    kos_term* depositaccountinnetherlands_type = kos_mk_prop("DepositAccountInNetherlands");
    kos_ontology_add_type_definition(ontology, "DepositAccountInNetherlands", depositaccountinnetherlands_type, NULL);
    // DepositAccountInBelgium
    kos_term* depositaccountinbelgium_type = kos_mk_prop("DepositAccountInBelgium");
    kos_ontology_add_type_definition(ontology, "DepositAccountInBelgium", depositaccountinbelgium_type, NULL);
    // DepositAccountInGermany
    kos_term* depositaccountingermany_type = kos_mk_prop("DepositAccountInGermany");
    kos_ontology_add_type_definition(ontology, "DepositAccountInGermany", depositaccountingermany_type, NULL);
    // InvestmentAccountInUnitedStates
    kos_term* investmentaccountinunitedstates_type = kos_mk_prop("InvestmentAccountInUnitedStates");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInUnitedStates", investmentaccountinunitedstates_type, NULL);
    // InvestmentAccountInUnitedKingdom
    kos_term* investmentaccountinunitedkingdom_type = kos_mk_prop("InvestmentAccountInUnitedKingdom");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInUnitedKingdom", investmentaccountinunitedkingdom_type, NULL);
    // InvestmentAccountInSwitzerland
    kos_term* investmentaccountinswitzerland_type = kos_mk_prop("InvestmentAccountInSwitzerland");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInSwitzerland", investmentaccountinswitzerland_type, NULL);
    // InvestmentAccountInSingapore
    kos_term* investmentaccountinsingapore_type = kos_mk_prop("InvestmentAccountInSingapore");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInSingapore", investmentaccountinsingapore_type, NULL);
    // InvestmentAccountInHongKong
    kos_term* investmentaccountinhongkong_type = kos_mk_prop("InvestmentAccountInHongKong");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInHongKong", investmentaccountinhongkong_type, NULL);
    // InvestmentAccountInLuxembourg
    kos_term* investmentaccountinluxembourg_type = kos_mk_prop("InvestmentAccountInLuxembourg");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInLuxembourg", investmentaccountinluxembourg_type, NULL);
    // InvestmentAccountInCaymanIslands
    kos_term* investmentaccountincaymanislands_type = kos_mk_prop("InvestmentAccountInCaymanIslands");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInCaymanIslands", investmentaccountincaymanislands_type, NULL);
    // InvestmentAccountInBritishVirginIslands
    kos_term* investmentaccountinbritishvirginislands_type = kos_mk_prop("InvestmentAccountInBritishVirginIslands");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInBritishVirginIslands", investmentaccountinbritishvirginislands_type, NULL);
    // InvestmentAccountInBermuda
    kos_term* investmentaccountinbermuda_type = kos_mk_prop("InvestmentAccountInBermuda");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInBermuda", investmentaccountinbermuda_type, NULL);
    // InvestmentAccountInPanama
    kos_term* investmentaccountinpanama_type = kos_mk_prop("InvestmentAccountInPanama");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInPanama", investmentaccountinpanama_type, NULL);
    // InvestmentAccountInBahamas
    kos_term* investmentaccountinbahamas_type = kos_mk_prop("InvestmentAccountInBahamas");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInBahamas", investmentaccountinbahamas_type, NULL);
    // InvestmentAccountInJersey
    kos_term* investmentaccountinjersey_type = kos_mk_prop("InvestmentAccountInJersey");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInJersey", investmentaccountinjersey_type, NULL);
    // InvestmentAccountInGuernsey
    kos_term* investmentaccountinguernsey_type = kos_mk_prop("InvestmentAccountInGuernsey");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInGuernsey", investmentaccountinguernsey_type, NULL);
    // InvestmentAccountInIsleOfMan
    kos_term* investmentaccountinisleofman_type = kos_mk_prop("InvestmentAccountInIsleOfMan");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInIsleOfMan", investmentaccountinisleofman_type, NULL);
    // InvestmentAccountInCyprus
    kos_term* investmentaccountincyprus_type = kos_mk_prop("InvestmentAccountInCyprus");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInCyprus", investmentaccountincyprus_type, NULL);
    // InvestmentAccountInMalta
    kos_term* investmentaccountinmalta_type = kos_mk_prop("InvestmentAccountInMalta");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInMalta", investmentaccountinmalta_type, NULL);
    // InvestmentAccountInIreland
    kos_term* investmentaccountinireland_type = kos_mk_prop("InvestmentAccountInIreland");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInIreland", investmentaccountinireland_type, NULL);
    // InvestmentAccountInNetherlands
    kos_term* investmentaccountinnetherlands_type = kos_mk_prop("InvestmentAccountInNetherlands");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInNetherlands", investmentaccountinnetherlands_type, NULL);
    // InvestmentAccountInBelgium
    kos_term* investmentaccountinbelgium_type = kos_mk_prop("InvestmentAccountInBelgium");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInBelgium", investmentaccountinbelgium_type, NULL);
    // InvestmentAccountInGermany
    kos_term* investmentaccountingermany_type = kos_mk_prop("InvestmentAccountInGermany");
    kos_ontology_add_type_definition(ontology, "InvestmentAccountInGermany", investmentaccountingermany_type, NULL);
    // TradingAccountInUnitedStates
    kos_term* tradingaccountinunitedstates_type = kos_mk_prop("TradingAccountInUnitedStates");
    kos_ontology_add_type_definition(ontology, "TradingAccountInUnitedStates", tradingaccountinunitedstates_type, NULL);
    // TradingAccountInUnitedKingdom
    kos_term* tradingaccountinunitedkingdom_type = kos_mk_prop("TradingAccountInUnitedKingdom");
    kos_ontology_add_type_definition(ontology, "TradingAccountInUnitedKingdom", tradingaccountinunitedkingdom_type, NULL);
    // TradingAccountInSwitzerland
    kos_term* tradingaccountinswitzerland_type = kos_mk_prop("TradingAccountInSwitzerland");
    kos_ontology_add_type_definition(ontology, "TradingAccountInSwitzerland", tradingaccountinswitzerland_type, NULL);
    // TradingAccountInSingapore
    kos_term* tradingaccountinsingapore_type = kos_mk_prop("TradingAccountInSingapore");
    kos_ontology_add_type_definition(ontology, "TradingAccountInSingapore", tradingaccountinsingapore_type, NULL);
    // TradingAccountInHongKong
    kos_term* tradingaccountinhongkong_type = kos_mk_prop("TradingAccountInHongKong");
    kos_ontology_add_type_definition(ontology, "TradingAccountInHongKong", tradingaccountinhongkong_type, NULL);
    // TradingAccountInLuxembourg
    kos_term* tradingaccountinluxembourg_type = kos_mk_prop("TradingAccountInLuxembourg");
    kos_ontology_add_type_definition(ontology, "TradingAccountInLuxembourg", tradingaccountinluxembourg_type, NULL);
    // TradingAccountInCaymanIslands
    kos_term* tradingaccountincaymanislands_type = kos_mk_prop("TradingAccountInCaymanIslands");
    kos_ontology_add_type_definition(ontology, "TradingAccountInCaymanIslands", tradingaccountincaymanislands_type, NULL);
    // TradingAccountInBritishVirginIslands
    kos_term* tradingaccountinbritishvirginislands_type = kos_mk_prop("TradingAccountInBritishVirginIslands");
    kos_ontology_add_type_definition(ontology, "TradingAccountInBritishVirginIslands", tradingaccountinbritishvirginislands_type, NULL);
    // TradingAccountInBermuda
    kos_term* tradingaccountinbermuda_type = kos_mk_prop("TradingAccountInBermuda");
    kos_ontology_add_type_definition(ontology, "TradingAccountInBermuda", tradingaccountinbermuda_type, NULL);
    // TradingAccountInPanama
    kos_term* tradingaccountinpanama_type = kos_mk_prop("TradingAccountInPanama");
    kos_ontology_add_type_definition(ontology, "TradingAccountInPanama", tradingaccountinpanama_type, NULL);
    // TradingAccountInBahamas
    kos_term* tradingaccountinbahamas_type = kos_mk_prop("TradingAccountInBahamas");
    kos_ontology_add_type_definition(ontology, "TradingAccountInBahamas", tradingaccountinbahamas_type, NULL);
    // TradingAccountInJersey
    kos_term* tradingaccountinjersey_type = kos_mk_prop("TradingAccountInJersey");
    kos_ontology_add_type_definition(ontology, "TradingAccountInJersey", tradingaccountinjersey_type, NULL);
    // TradingAccountInGuernsey
    kos_term* tradingaccountinguernsey_type = kos_mk_prop("TradingAccountInGuernsey");
    kos_ontology_add_type_definition(ontology, "TradingAccountInGuernsey", tradingaccountinguernsey_type, NULL);
    // TradingAccountInIsleOfMan
    kos_term* tradingaccountinisleofman_type = kos_mk_prop("TradingAccountInIsleOfMan");
    kos_ontology_add_type_definition(ontology, "TradingAccountInIsleOfMan", tradingaccountinisleofman_type, NULL);
    // TradingAccountInCyprus
    kos_term* tradingaccountincyprus_type = kos_mk_prop("TradingAccountInCyprus");
    kos_ontology_add_type_definition(ontology, "TradingAccountInCyprus", tradingaccountincyprus_type, NULL);
    // TradingAccountInMalta
    kos_term* tradingaccountinmalta_type = kos_mk_prop("TradingAccountInMalta");
    kos_ontology_add_type_definition(ontology, "TradingAccountInMalta", tradingaccountinmalta_type, NULL);
    // TradingAccountInIreland
    kos_term* tradingaccountinireland_type = kos_mk_prop("TradingAccountInIreland");
    kos_ontology_add_type_definition(ontology, "TradingAccountInIreland", tradingaccountinireland_type, NULL);
    // TradingAccountInNetherlands
    kos_term* tradingaccountinnetherlands_type = kos_mk_prop("TradingAccountInNetherlands");
    kos_ontology_add_type_definition(ontology, "TradingAccountInNetherlands", tradingaccountinnetherlands_type, NULL);
    // TradingAccountInBelgium
    kos_term* tradingaccountinbelgium_type = kos_mk_prop("TradingAccountInBelgium");
    kos_ontology_add_type_definition(ontology, "TradingAccountInBelgium", tradingaccountinbelgium_type, NULL);
    // TradingAccountInGermany
    kos_term* tradingaccountingermany_type = kos_mk_prop("TradingAccountInGermany");
    kos_ontology_add_type_definition(ontology, "TradingAccountInGermany", tradingaccountingermany_type, NULL);
    // MarginAccountInUnitedStates
    kos_term* marginaccountinunitedstates_type = kos_mk_prop("MarginAccountInUnitedStates");
    kos_ontology_add_type_definition(ontology, "MarginAccountInUnitedStates", marginaccountinunitedstates_type, NULL);
    // MarginAccountInUnitedKingdom
    kos_term* marginaccountinunitedkingdom_type = kos_mk_prop("MarginAccountInUnitedKingdom");
    kos_ontology_add_type_definition(ontology, "MarginAccountInUnitedKingdom", marginaccountinunitedkingdom_type, NULL);
    // MarginAccountInSwitzerland
    kos_term* marginaccountinswitzerland_type = kos_mk_prop("MarginAccountInSwitzerland");
    kos_ontology_add_type_definition(ontology, "MarginAccountInSwitzerland", marginaccountinswitzerland_type, NULL);
    // MarginAccountInSingapore
    kos_term* marginaccountinsingapore_type = kos_mk_prop("MarginAccountInSingapore");
    kos_ontology_add_type_definition(ontology, "MarginAccountInSingapore", marginaccountinsingapore_type, NULL);
    // MarginAccountInHongKong
    kos_term* marginaccountinhongkong_type = kos_mk_prop("MarginAccountInHongKong");
    kos_ontology_add_type_definition(ontology, "MarginAccountInHongKong", marginaccountinhongkong_type, NULL);
    // MarginAccountInLuxembourg
    kos_term* marginaccountinluxembourg_type = kos_mk_prop("MarginAccountInLuxembourg");
    kos_ontology_add_type_definition(ontology, "MarginAccountInLuxembourg", marginaccountinluxembourg_type, NULL);
    // MarginAccountInCaymanIslands
    kos_term* marginaccountincaymanislands_type = kos_mk_prop("MarginAccountInCaymanIslands");
    kos_ontology_add_type_definition(ontology, "MarginAccountInCaymanIslands", marginaccountincaymanislands_type, NULL);
    // MarginAccountInBritishVirginIslands
    kos_term* marginaccountinbritishvirginislands_type = kos_mk_prop("MarginAccountInBritishVirginIslands");
    kos_ontology_add_type_definition(ontology, "MarginAccountInBritishVirginIslands", marginaccountinbritishvirginislands_type, NULL);
    // MarginAccountInBermuda
    kos_term* marginaccountinbermuda_type = kos_mk_prop("MarginAccountInBermuda");
    kos_ontology_add_type_definition(ontology, "MarginAccountInBermuda", marginaccountinbermuda_type, NULL);
    // MarginAccountInPanama
    kos_term* marginaccountinpanama_type = kos_mk_prop("MarginAccountInPanama");
    kos_ontology_add_type_definition(ontology, "MarginAccountInPanama", marginaccountinpanama_type, NULL);
    // MarginAccountInBahamas
    kos_term* marginaccountinbahamas_type = kos_mk_prop("MarginAccountInBahamas");
    kos_ontology_add_type_definition(ontology, "MarginAccountInBahamas", marginaccountinbahamas_type, NULL);
    // MarginAccountInJersey
    kos_term* marginaccountinjersey_type = kos_mk_prop("MarginAccountInJersey");
    kos_ontology_add_type_definition(ontology, "MarginAccountInJersey", marginaccountinjersey_type, NULL);
    // MarginAccountInGuernsey
    kos_term* marginaccountinguernsey_type = kos_mk_prop("MarginAccountInGuernsey");
    kos_ontology_add_type_definition(ontology, "MarginAccountInGuernsey", marginaccountinguernsey_type, NULL);
    // MarginAccountInIsleOfMan
    kos_term* marginaccountinisleofman_type = kos_mk_prop("MarginAccountInIsleOfMan");
    kos_ontology_add_type_definition(ontology, "MarginAccountInIsleOfMan", marginaccountinisleofman_type, NULL);
    // MarginAccountInCyprus
    kos_term* marginaccountincyprus_type = kos_mk_prop("MarginAccountInCyprus");
    kos_ontology_add_type_definition(ontology, "MarginAccountInCyprus", marginaccountincyprus_type, NULL);
    // MarginAccountInMalta
    kos_term* marginaccountinmalta_type = kos_mk_prop("MarginAccountInMalta");
    kos_ontology_add_type_definition(ontology, "MarginAccountInMalta", marginaccountinmalta_type, NULL);
    // MarginAccountInIreland
    kos_term* marginaccountinireland_type = kos_mk_prop("MarginAccountInIreland");
    kos_ontology_add_type_definition(ontology, "MarginAccountInIreland", marginaccountinireland_type, NULL);
    // MarginAccountInNetherlands
    kos_term* marginaccountinnetherlands_type = kos_mk_prop("MarginAccountInNetherlands");
    kos_ontology_add_type_definition(ontology, "MarginAccountInNetherlands", marginaccountinnetherlands_type, NULL);
    // MarginAccountInBelgium
    kos_term* marginaccountinbelgium_type = kos_mk_prop("MarginAccountInBelgium");
    kos_ontology_add_type_definition(ontology, "MarginAccountInBelgium", marginaccountinbelgium_type, NULL);
    // MarginAccountInGermany
    kos_term* marginaccountingermany_type = kos_mk_prop("MarginAccountInGermany");
    kos_ontology_add_type_definition(ontology, "MarginAccountInGermany", marginaccountingermany_type, NULL);
    // ForexAccountInUnitedStates
    kos_term* forexaccountinunitedstates_type = kos_mk_prop("ForexAccountInUnitedStates");
    kos_ontology_add_type_definition(ontology, "ForexAccountInUnitedStates", forexaccountinunitedstates_type, NULL);
    // ForexAccountInUnitedKingdom
    kos_term* forexaccountinunitedkingdom_type = kos_mk_prop("ForexAccountInUnitedKingdom");
    kos_ontology_add_type_definition(ontology, "ForexAccountInUnitedKingdom", forexaccountinunitedkingdom_type, NULL);
    // ForexAccountInSwitzerland
    kos_term* forexaccountinswitzerland_type = kos_mk_prop("ForexAccountInSwitzerland");
    kos_ontology_add_type_definition(ontology, "ForexAccountInSwitzerland", forexaccountinswitzerland_type, NULL);
    // ForexAccountInSingapore
    kos_term* forexaccountinsingapore_type = kos_mk_prop("ForexAccountInSingapore");
    kos_ontology_add_type_definition(ontology, "ForexAccountInSingapore", forexaccountinsingapore_type, NULL);
    // ForexAccountInHongKong
    kos_term* forexaccountinhongkong_type = kos_mk_prop("ForexAccountInHongKong");
    kos_ontology_add_type_definition(ontology, "ForexAccountInHongKong", forexaccountinhongkong_type, NULL);
    // ForexAccountInLuxembourg
    kos_term* forexaccountinluxembourg_type = kos_mk_prop("ForexAccountInLuxembourg");
    kos_ontology_add_type_definition(ontology, "ForexAccountInLuxembourg", forexaccountinluxembourg_type, NULL);
    // ForexAccountInCaymanIslands
    kos_term* forexaccountincaymanislands_type = kos_mk_prop("ForexAccountInCaymanIslands");
    kos_ontology_add_type_definition(ontology, "ForexAccountInCaymanIslands", forexaccountincaymanislands_type, NULL);
    // ForexAccountInBritishVirginIslands
    kos_term* forexaccountinbritishvirginislands_type = kos_mk_prop("ForexAccountInBritishVirginIslands");
    kos_ontology_add_type_definition(ontology, "ForexAccountInBritishVirginIslands", forexaccountinbritishvirginislands_type, NULL);
    // ForexAccountInBermuda
    kos_term* forexaccountinbermuda_type = kos_mk_prop("ForexAccountInBermuda");
    kos_ontology_add_type_definition(ontology, "ForexAccountInBermuda", forexaccountinbermuda_type, NULL);
    // ForexAccountInPanama
    kos_term* forexaccountinpanama_type = kos_mk_prop("ForexAccountInPanama");
    kos_ontology_add_type_definition(ontology, "ForexAccountInPanama", forexaccountinpanama_type, NULL);
    // ForexAccountInBahamas
    kos_term* forexaccountinbahamas_type = kos_mk_prop("ForexAccountInBahamas");
    kos_ontology_add_type_definition(ontology, "ForexAccountInBahamas", forexaccountinbahamas_type, NULL);
    // ForexAccountInJersey
    kos_term* forexaccountinjersey_type = kos_mk_prop("ForexAccountInJersey");
    kos_ontology_add_type_definition(ontology, "ForexAccountInJersey", forexaccountinjersey_type, NULL);
    // ForexAccountInGuernsey
    kos_term* forexaccountinguernsey_type = kos_mk_prop("ForexAccountInGuernsey");
    kos_ontology_add_type_definition(ontology, "ForexAccountInGuernsey", forexaccountinguernsey_type, NULL);
    // ForexAccountInIsleOfMan
    kos_term* forexaccountinisleofman_type = kos_mk_prop("ForexAccountInIsleOfMan");
    kos_ontology_add_type_definition(ontology, "ForexAccountInIsleOfMan", forexaccountinisleofman_type, NULL);
    // ForexAccountInCyprus
    kos_term* forexaccountincyprus_type = kos_mk_prop("ForexAccountInCyprus");
    kos_ontology_add_type_definition(ontology, "ForexAccountInCyprus", forexaccountincyprus_type, NULL);
    // ForexAccountInMalta
    kos_term* forexaccountinmalta_type = kos_mk_prop("ForexAccountInMalta");
    kos_ontology_add_type_definition(ontology, "ForexAccountInMalta", forexaccountinmalta_type, NULL);
    // ForexAccountInIreland
    kos_term* forexaccountinireland_type = kos_mk_prop("ForexAccountInIreland");
    kos_ontology_add_type_definition(ontology, "ForexAccountInIreland", forexaccountinireland_type, NULL);
    // ForexAccountInNetherlands
    kos_term* forexaccountinnetherlands_type = kos_mk_prop("ForexAccountInNetherlands");
    kos_ontology_add_type_definition(ontology, "ForexAccountInNetherlands", forexaccountinnetherlands_type, NULL);
    // ForexAccountInBelgium
    kos_term* forexaccountinbelgium_type = kos_mk_prop("ForexAccountInBelgium");
    kos_ontology_add_type_definition(ontology, "ForexAccountInBelgium", forexaccountinbelgium_type, NULL);
    // ForexAccountInGermany
    kos_term* forexaccountingermany_type = kos_mk_prop("ForexAccountInGermany");
    kos_ontology_add_type_definition(ontology, "ForexAccountInGermany", forexaccountingermany_type, NULL);
    // CryptocurrencyAccountInUnitedStates
    kos_term* cryptocurrencyaccountinunitedstates_type = kos_mk_prop("CryptocurrencyAccountInUnitedStates");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInUnitedStates", cryptocurrencyaccountinunitedstates_type, NULL);
    // CryptocurrencyAccountInUnitedKingdom
    kos_term* cryptocurrencyaccountinunitedkingdom_type = kos_mk_prop("CryptocurrencyAccountInUnitedKingdom");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInUnitedKingdom", cryptocurrencyaccountinunitedkingdom_type, NULL);
    // CryptocurrencyAccountInSwitzerland
    kos_term* cryptocurrencyaccountinswitzerland_type = kos_mk_prop("CryptocurrencyAccountInSwitzerland");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInSwitzerland", cryptocurrencyaccountinswitzerland_type, NULL);
    // CryptocurrencyAccountInSingapore
    kos_term* cryptocurrencyaccountinsingapore_type = kos_mk_prop("CryptocurrencyAccountInSingapore");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInSingapore", cryptocurrencyaccountinsingapore_type, NULL);
    // CryptocurrencyAccountInHongKong
    kos_term* cryptocurrencyaccountinhongkong_type = kos_mk_prop("CryptocurrencyAccountInHongKong");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInHongKong", cryptocurrencyaccountinhongkong_type, NULL);
    // CryptocurrencyAccountInLuxembourg
    kos_term* cryptocurrencyaccountinluxembourg_type = kos_mk_prop("CryptocurrencyAccountInLuxembourg");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInLuxembourg", cryptocurrencyaccountinluxembourg_type, NULL);
    // CryptocurrencyAccountInCaymanIslands
    kos_term* cryptocurrencyaccountincaymanislands_type = kos_mk_prop("CryptocurrencyAccountInCaymanIslands");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInCaymanIslands", cryptocurrencyaccountincaymanislands_type, NULL);
    // CryptocurrencyAccountInBritishVirginIslands
    kos_term* cryptocurrencyaccountinbritishvirginislands_type = kos_mk_prop("CryptocurrencyAccountInBritishVirginIslands");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInBritishVirginIslands", cryptocurrencyaccountinbritishvirginislands_type, NULL);
    // CryptocurrencyAccountInBermuda
    kos_term* cryptocurrencyaccountinbermuda_type = kos_mk_prop("CryptocurrencyAccountInBermuda");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInBermuda", cryptocurrencyaccountinbermuda_type, NULL);
    // CryptocurrencyAccountInPanama
    kos_term* cryptocurrencyaccountinpanama_type = kos_mk_prop("CryptocurrencyAccountInPanama");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInPanama", cryptocurrencyaccountinpanama_type, NULL);
    // CryptocurrencyAccountInBahamas
    kos_term* cryptocurrencyaccountinbahamas_type = kos_mk_prop("CryptocurrencyAccountInBahamas");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInBahamas", cryptocurrencyaccountinbahamas_type, NULL);
    // CryptocurrencyAccountInJersey
    kos_term* cryptocurrencyaccountinjersey_type = kos_mk_prop("CryptocurrencyAccountInJersey");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInJersey", cryptocurrencyaccountinjersey_type, NULL);
    // CryptocurrencyAccountInGuernsey
    kos_term* cryptocurrencyaccountinguernsey_type = kos_mk_prop("CryptocurrencyAccountInGuernsey");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInGuernsey", cryptocurrencyaccountinguernsey_type, NULL);
    // CryptocurrencyAccountInIsleOfMan
    kos_term* cryptocurrencyaccountinisleofman_type = kos_mk_prop("CryptocurrencyAccountInIsleOfMan");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInIsleOfMan", cryptocurrencyaccountinisleofman_type, NULL);
    // CryptocurrencyAccountInCyprus
    kos_term* cryptocurrencyaccountincyprus_type = kos_mk_prop("CryptocurrencyAccountInCyprus");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInCyprus", cryptocurrencyaccountincyprus_type, NULL);
    // CryptocurrencyAccountInMalta
    kos_term* cryptocurrencyaccountinmalta_type = kos_mk_prop("CryptocurrencyAccountInMalta");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInMalta", cryptocurrencyaccountinmalta_type, NULL);
    // CryptocurrencyAccountInIreland
    kos_term* cryptocurrencyaccountinireland_type = kos_mk_prop("CryptocurrencyAccountInIreland");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInIreland", cryptocurrencyaccountinireland_type, NULL);
    // CryptocurrencyAccountInNetherlands
    kos_term* cryptocurrencyaccountinnetherlands_type = kos_mk_prop("CryptocurrencyAccountInNetherlands");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInNetherlands", cryptocurrencyaccountinnetherlands_type, NULL);
    // CryptocurrencyAccountInBelgium
    kos_term* cryptocurrencyaccountinbelgium_type = kos_mk_prop("CryptocurrencyAccountInBelgium");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInBelgium", cryptocurrencyaccountinbelgium_type, NULL);
    // CryptocurrencyAccountInGermany
    kos_term* cryptocurrencyaccountingermany_type = kos_mk_prop("CryptocurrencyAccountInGermany");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyAccountInGermany", cryptocurrencyaccountingermany_type, NULL);
    // OffshoreAccountInUnitedStates
    kos_term* offshoreaccountinunitedstates_type = kos_mk_prop("OffshoreAccountInUnitedStates");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInUnitedStates", offshoreaccountinunitedstates_type, NULL);
    // OffshoreAccountInUnitedKingdom
    kos_term* offshoreaccountinunitedkingdom_type = kos_mk_prop("OffshoreAccountInUnitedKingdom");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInUnitedKingdom", offshoreaccountinunitedkingdom_type, NULL);
    // OffshoreAccountInSwitzerland
    kos_term* offshoreaccountinswitzerland_type = kos_mk_prop("OffshoreAccountInSwitzerland");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInSwitzerland", offshoreaccountinswitzerland_type, NULL);
    // OffshoreAccountInSingapore
    kos_term* offshoreaccountinsingapore_type = kos_mk_prop("OffshoreAccountInSingapore");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInSingapore", offshoreaccountinsingapore_type, NULL);
    // OffshoreAccountInHongKong
    kos_term* offshoreaccountinhongkong_type = kos_mk_prop("OffshoreAccountInHongKong");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInHongKong", offshoreaccountinhongkong_type, NULL);
    // OffshoreAccountInLuxembourg
    kos_term* offshoreaccountinluxembourg_type = kos_mk_prop("OffshoreAccountInLuxembourg");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInLuxembourg", offshoreaccountinluxembourg_type, NULL);
    // OffshoreAccountInCaymanIslands
    kos_term* offshoreaccountincaymanislands_type = kos_mk_prop("OffshoreAccountInCaymanIslands");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInCaymanIslands", offshoreaccountincaymanislands_type, NULL);
    // OffshoreAccountInBritishVirginIslands
    kos_term* offshoreaccountinbritishvirginislands_type = kos_mk_prop("OffshoreAccountInBritishVirginIslands");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInBritishVirginIslands", offshoreaccountinbritishvirginislands_type, NULL);
    // OffshoreAccountInBermuda
    kos_term* offshoreaccountinbermuda_type = kos_mk_prop("OffshoreAccountInBermuda");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInBermuda", offshoreaccountinbermuda_type, NULL);
    // OffshoreAccountInPanama
    kos_term* offshoreaccountinpanama_type = kos_mk_prop("OffshoreAccountInPanama");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInPanama", offshoreaccountinpanama_type, NULL);
    // OffshoreAccountInBahamas
    kos_term* offshoreaccountinbahamas_type = kos_mk_prop("OffshoreAccountInBahamas");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInBahamas", offshoreaccountinbahamas_type, NULL);
    // OffshoreAccountInJersey
    kos_term* offshoreaccountinjersey_type = kos_mk_prop("OffshoreAccountInJersey");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInJersey", offshoreaccountinjersey_type, NULL);
    // OffshoreAccountInGuernsey
    kos_term* offshoreaccountinguernsey_type = kos_mk_prop("OffshoreAccountInGuernsey");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInGuernsey", offshoreaccountinguernsey_type, NULL);
    // OffshoreAccountInIsleOfMan
    kos_term* offshoreaccountinisleofman_type = kos_mk_prop("OffshoreAccountInIsleOfMan");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInIsleOfMan", offshoreaccountinisleofman_type, NULL);
    // OffshoreAccountInCyprus
    kos_term* offshoreaccountincyprus_type = kos_mk_prop("OffshoreAccountInCyprus");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInCyprus", offshoreaccountincyprus_type, NULL);
    // OffshoreAccountInMalta
    kos_term* offshoreaccountinmalta_type = kos_mk_prop("OffshoreAccountInMalta");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInMalta", offshoreaccountinmalta_type, NULL);
    // OffshoreAccountInIreland
    kos_term* offshoreaccountinireland_type = kos_mk_prop("OffshoreAccountInIreland");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInIreland", offshoreaccountinireland_type, NULL);
    // OffshoreAccountInNetherlands
    kos_term* offshoreaccountinnetherlands_type = kos_mk_prop("OffshoreAccountInNetherlands");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInNetherlands", offshoreaccountinnetherlands_type, NULL);
    // OffshoreAccountInBelgium
    kos_term* offshoreaccountinbelgium_type = kos_mk_prop("OffshoreAccountInBelgium");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInBelgium", offshoreaccountinbelgium_type, NULL);
    // OffshoreAccountInGermany
    kos_term* offshoreaccountingermany_type = kos_mk_prop("OffshoreAccountInGermany");
    kos_ontology_add_type_definition(ontology, "OffshoreAccountInGermany", offshoreaccountingermany_type, NULL);

    // ========== 交易模式类型 ==========
    // WireTransferWithLowRisk
    kos_term* wiretransferwithlowrisk_type = kos_mk_prop("WireTransferWithLowRisk");
    kos_ontology_add_type_definition(ontology, "WireTransferWithLowRisk", wiretransferwithlowrisk_type, NULL);
    // WireTransferWithMediumRisk
    kos_term* wiretransferwithmediumrisk_type = kos_mk_prop("WireTransferWithMediumRisk");
    kos_ontology_add_type_definition(ontology, "WireTransferWithMediumRisk", wiretransferwithmediumrisk_type, NULL);
    // WireTransferWithHighRisk
    kos_term* wiretransferwithhighrisk_type = kos_mk_prop("WireTransferWithHighRisk");
    kos_ontology_add_type_definition(ontology, "WireTransferWithHighRisk", wiretransferwithhighrisk_type, NULL);
    // WireTransferWithVeryHighRisk
    kos_term* wiretransferwithveryhighrisk_type = kos_mk_prop("WireTransferWithVeryHighRisk");
    kos_ontology_add_type_definition(ontology, "WireTransferWithVeryHighRisk", wiretransferwithveryhighrisk_type, NULL);
    // WireTransferWithCriticalRisk
    kos_term* wiretransferwithcriticalrisk_type = kos_mk_prop("WireTransferWithCriticalRisk");
    kos_ontology_add_type_definition(ontology, "WireTransferWithCriticalRisk", wiretransferwithcriticalrisk_type, NULL);
    // WireTransferWithSanctioned
    kos_term* wiretransferwithsanctioned_type = kos_mk_prop("WireTransferWithSanctioned");
    kos_ontology_add_type_definition(ontology, "WireTransferWithSanctioned", wiretransferwithsanctioned_type, NULL);
    // WireTransferWithPEP
    kos_term* wiretransferwithpep_type = kos_mk_prop("WireTransferWithPEP");
    kos_ontology_add_type_definition(ontology, "WireTransferWithPEP", wiretransferwithpep_type, NULL);
    // WireTransferWithAdverseMedia
    kos_term* wiretransferwithadversemedia_type = kos_mk_prop("WireTransferWithAdverseMedia");
    kos_ontology_add_type_definition(ontology, "WireTransferWithAdverseMedia", wiretransferwithadversemedia_type, NULL);
    // WireTransferWithSuspiciousActivity
    kos_term* wiretransferwithsuspiciousactivity_type = kos_mk_prop("WireTransferWithSuspiciousActivity");
    kos_ontology_add_type_definition(ontology, "WireTransferWithSuspiciousActivity", wiretransferwithsuspiciousactivity_type, NULL);
    // WireTransferWithTerroristFinancing
    kos_term* wiretransferwithterroristfinancing_type = kos_mk_prop("WireTransferWithTerroristFinancing");
    kos_ontology_add_type_definition(ontology, "WireTransferWithTerroristFinancing", wiretransferwithterroristfinancing_type, NULL);
    // WireTransferWithDrugTrafficking
    kos_term* wiretransferwithdrugtrafficking_type = kos_mk_prop("WireTransferWithDrugTrafficking");
    kos_ontology_add_type_definition(ontology, "WireTransferWithDrugTrafficking", wiretransferwithdrugtrafficking_type, NULL);
    // WireTransferWithHumanTrafficking
    kos_term* wiretransferwithhumantrafficking_type = kos_mk_prop("WireTransferWithHumanTrafficking");
    kos_ontology_add_type_definition(ontology, "WireTransferWithHumanTrafficking", wiretransferwithhumantrafficking_type, NULL);
    // WireTransferWithCorruption
    kos_term* wiretransferwithcorruption_type = kos_mk_prop("WireTransferWithCorruption");
    kos_ontology_add_type_definition(ontology, "WireTransferWithCorruption", wiretransferwithcorruption_type, NULL);
    // WireTransferWithTaxEvasion
    kos_term* wiretransferwithtaxevasion_type = kos_mk_prop("WireTransferWithTaxEvasion");
    kos_ontology_add_type_definition(ontology, "WireTransferWithTaxEvasion", wiretransferwithtaxevasion_type, NULL);
    // WireTransferWithMarketManipulation
    kos_term* wiretransferwithmarketmanipulation_type = kos_mk_prop("WireTransferWithMarketManipulation");
    kos_ontology_add_type_definition(ontology, "WireTransferWithMarketManipulation", wiretransferwithmarketmanipulation_type, NULL);
    // WireTransferWithInsiderTrading
    kos_term* wiretransferwithinsidertrading_type = kos_mk_prop("WireTransferWithInsiderTrading");
    kos_ontology_add_type_definition(ontology, "WireTransferWithInsiderTrading", wiretransferwithinsidertrading_type, NULL);
    // ACHTransferWithLowRisk
    kos_term* achtransferwithlowrisk_type = kos_mk_prop("ACHTransferWithLowRisk");
    kos_ontology_add_type_definition(ontology, "ACHTransferWithLowRisk", achtransferwithlowrisk_type, NULL);
    // ACHTransferWithMediumRisk
    kos_term* achtransferwithmediumrisk_type = kos_mk_prop("ACHTransferWithMediumRisk");
    kos_ontology_add_type_definition(ontology, "ACHTransferWithMediumRisk", achtransferwithmediumrisk_type, NULL);
    // ACHTransferWithHighRisk
    kos_term* achtransferwithhighrisk_type = kos_mk_prop("ACHTransferWithHighRisk");
    kos_ontology_add_type_definition(ontology, "ACHTransferWithHighRisk", achtransferwithhighrisk_type, NULL);
    // ACHTransferWithVeryHighRisk
    kos_term* achtransferwithveryhighrisk_type = kos_mk_prop("ACHTransferWithVeryHighRisk");
    kos_ontology_add_type_definition(ontology, "ACHTransferWithVeryHighRisk", achtransferwithveryhighrisk_type, NULL);
    // ACHTransferWithCriticalRisk
    kos_term* achtransferwithcriticalrisk_type = kos_mk_prop("ACHTransferWithCriticalRisk");
    kos_ontology_add_type_definition(ontology, "ACHTransferWithCriticalRisk", achtransferwithcriticalrisk_type, NULL);
    // ACHTransferWithSanctioned
    kos_term* achtransferwithsanctioned_type = kos_mk_prop("ACHTransferWithSanctioned");
    kos_ontology_add_type_definition(ontology, "ACHTransferWithSanctioned", achtransferwithsanctioned_type, NULL);
    // ACHTransferWithPEP
    kos_term* achtransferwithpep_type = kos_mk_prop("ACHTransferWithPEP");
    kos_ontology_add_type_definition(ontology, "ACHTransferWithPEP", achtransferwithpep_type, NULL);
    // ACHTransferWithAdverseMedia
    kos_term* achtransferwithadversemedia_type = kos_mk_prop("ACHTransferWithAdverseMedia");
    kos_ontology_add_type_definition(ontology, "ACHTransferWithAdverseMedia", achtransferwithadversemedia_type, NULL);
    // ACHTransferWithSuspiciousActivity
    kos_term* achtransferwithsuspiciousactivity_type = kos_mk_prop("ACHTransferWithSuspiciousActivity");
    kos_ontology_add_type_definition(ontology, "ACHTransferWithSuspiciousActivity", achtransferwithsuspiciousactivity_type, NULL);
    // ACHTransferWithTerroristFinancing
    kos_term* achtransferwithterroristfinancing_type = kos_mk_prop("ACHTransferWithTerroristFinancing");
    kos_ontology_add_type_definition(ontology, "ACHTransferWithTerroristFinancing", achtransferwithterroristfinancing_type, NULL);
    // ACHTransferWithDrugTrafficking
    kos_term* achtransferwithdrugtrafficking_type = kos_mk_prop("ACHTransferWithDrugTrafficking");
    kos_ontology_add_type_definition(ontology, "ACHTransferWithDrugTrafficking", achtransferwithdrugtrafficking_type, NULL);
    // ACHTransferWithHumanTrafficking
    kos_term* achtransferwithhumantrafficking_type = kos_mk_prop("ACHTransferWithHumanTrafficking");
    kos_ontology_add_type_definition(ontology, "ACHTransferWithHumanTrafficking", achtransferwithhumantrafficking_type, NULL);
    // ACHTransferWithCorruption
    kos_term* achtransferwithcorruption_type = kos_mk_prop("ACHTransferWithCorruption");
    kos_ontology_add_type_definition(ontology, "ACHTransferWithCorruption", achtransferwithcorruption_type, NULL);
    // ACHTransferWithTaxEvasion
    kos_term* achtransferwithtaxevasion_type = kos_mk_prop("ACHTransferWithTaxEvasion");
    kos_ontology_add_type_definition(ontology, "ACHTransferWithTaxEvasion", achtransferwithtaxevasion_type, NULL);
    // ACHTransferWithMarketManipulation
    kos_term* achtransferwithmarketmanipulation_type = kos_mk_prop("ACHTransferWithMarketManipulation");
    kos_ontology_add_type_definition(ontology, "ACHTransferWithMarketManipulation", achtransferwithmarketmanipulation_type, NULL);
    // ACHTransferWithInsiderTrading
    kos_term* achtransferwithinsidertrading_type = kos_mk_prop("ACHTransferWithInsiderTrading");
    kos_ontology_add_type_definition(ontology, "ACHTransferWithInsiderTrading", achtransferwithinsidertrading_type, NULL);
    // SWIFTTransferWithLowRisk
    kos_term* swifttransferwithlowrisk_type = kos_mk_prop("SWIFTTransferWithLowRisk");
    kos_ontology_add_type_definition(ontology, "SWIFTTransferWithLowRisk", swifttransferwithlowrisk_type, NULL);
    // SWIFTTransferWithMediumRisk
    kos_term* swifttransferwithmediumrisk_type = kos_mk_prop("SWIFTTransferWithMediumRisk");
    kos_ontology_add_type_definition(ontology, "SWIFTTransferWithMediumRisk", swifttransferwithmediumrisk_type, NULL);
    // SWIFTTransferWithHighRisk
    kos_term* swifttransferwithhighrisk_type = kos_mk_prop("SWIFTTransferWithHighRisk");
    kos_ontology_add_type_definition(ontology, "SWIFTTransferWithHighRisk", swifttransferwithhighrisk_type, NULL);
    // SWIFTTransferWithVeryHighRisk
    kos_term* swifttransferwithveryhighrisk_type = kos_mk_prop("SWIFTTransferWithVeryHighRisk");
    kos_ontology_add_type_definition(ontology, "SWIFTTransferWithVeryHighRisk", swifttransferwithveryhighrisk_type, NULL);
    // SWIFTTransferWithCriticalRisk
    kos_term* swifttransferwithcriticalrisk_type = kos_mk_prop("SWIFTTransferWithCriticalRisk");
    kos_ontology_add_type_definition(ontology, "SWIFTTransferWithCriticalRisk", swifttransferwithcriticalrisk_type, NULL);
    // SWIFTTransferWithSanctioned
    kos_term* swifttransferwithsanctioned_type = kos_mk_prop("SWIFTTransferWithSanctioned");
    kos_ontology_add_type_definition(ontology, "SWIFTTransferWithSanctioned", swifttransferwithsanctioned_type, NULL);
    // SWIFTTransferWithPEP
    kos_term* swifttransferwithpep_type = kos_mk_prop("SWIFTTransferWithPEP");
    kos_ontology_add_type_definition(ontology, "SWIFTTransferWithPEP", swifttransferwithpep_type, NULL);
    // SWIFTTransferWithAdverseMedia
    kos_term* swifttransferwithadversemedia_type = kos_mk_prop("SWIFTTransferWithAdverseMedia");
    kos_ontology_add_type_definition(ontology, "SWIFTTransferWithAdverseMedia", swifttransferwithadversemedia_type, NULL);
    // SWIFTTransferWithSuspiciousActivity
    kos_term* swifttransferwithsuspiciousactivity_type = kos_mk_prop("SWIFTTransferWithSuspiciousActivity");
    kos_ontology_add_type_definition(ontology, "SWIFTTransferWithSuspiciousActivity", swifttransferwithsuspiciousactivity_type, NULL);
    // SWIFTTransferWithTerroristFinancing
    kos_term* swifttransferwithterroristfinancing_type = kos_mk_prop("SWIFTTransferWithTerroristFinancing");
    kos_ontology_add_type_definition(ontology, "SWIFTTransferWithTerroristFinancing", swifttransferwithterroristfinancing_type, NULL);
    // SWIFTTransferWithDrugTrafficking
    kos_term* swifttransferwithdrugtrafficking_type = kos_mk_prop("SWIFTTransferWithDrugTrafficking");
    kos_ontology_add_type_definition(ontology, "SWIFTTransferWithDrugTrafficking", swifttransferwithdrugtrafficking_type, NULL);
    // SWIFTTransferWithHumanTrafficking
    kos_term* swifttransferwithhumantrafficking_type = kos_mk_prop("SWIFTTransferWithHumanTrafficking");
    kos_ontology_add_type_definition(ontology, "SWIFTTransferWithHumanTrafficking", swifttransferwithhumantrafficking_type, NULL);
    // SWIFTTransferWithCorruption
    kos_term* swifttransferwithcorruption_type = kos_mk_prop("SWIFTTransferWithCorruption");
    kos_ontology_add_type_definition(ontology, "SWIFTTransferWithCorruption", swifttransferwithcorruption_type, NULL);
    // SWIFTTransferWithTaxEvasion
    kos_term* swifttransferwithtaxevasion_type = kos_mk_prop("SWIFTTransferWithTaxEvasion");
    kos_ontology_add_type_definition(ontology, "SWIFTTransferWithTaxEvasion", swifttransferwithtaxevasion_type, NULL);
    // SWIFTTransferWithMarketManipulation
    kos_term* swifttransferwithmarketmanipulation_type = kos_mk_prop("SWIFTTransferWithMarketManipulation");
    kos_ontology_add_type_definition(ontology, "SWIFTTransferWithMarketManipulation", swifttransferwithmarketmanipulation_type, NULL);
    // SWIFTTransferWithInsiderTrading
    kos_term* swifttransferwithinsidertrading_type = kos_mk_prop("SWIFTTransferWithInsiderTrading");
    kos_ontology_add_type_definition(ontology, "SWIFTTransferWithInsiderTrading", swifttransferwithinsidertrading_type, NULL);
    // CashDepositWithLowRisk
    kos_term* cashdepositwithlowrisk_type = kos_mk_prop("CashDepositWithLowRisk");
    kos_ontology_add_type_definition(ontology, "CashDepositWithLowRisk", cashdepositwithlowrisk_type, NULL);
    // CashDepositWithMediumRisk
    kos_term* cashdepositwithmediumrisk_type = kos_mk_prop("CashDepositWithMediumRisk");
    kos_ontology_add_type_definition(ontology, "CashDepositWithMediumRisk", cashdepositwithmediumrisk_type, NULL);
    // CashDepositWithHighRisk
    kos_term* cashdepositwithhighrisk_type = kos_mk_prop("CashDepositWithHighRisk");
    kos_ontology_add_type_definition(ontology, "CashDepositWithHighRisk", cashdepositwithhighrisk_type, NULL);
    // CashDepositWithVeryHighRisk
    kos_term* cashdepositwithveryhighrisk_type = kos_mk_prop("CashDepositWithVeryHighRisk");
    kos_ontology_add_type_definition(ontology, "CashDepositWithVeryHighRisk", cashdepositwithveryhighrisk_type, NULL);
    // CashDepositWithCriticalRisk
    kos_term* cashdepositwithcriticalrisk_type = kos_mk_prop("CashDepositWithCriticalRisk");
    kos_ontology_add_type_definition(ontology, "CashDepositWithCriticalRisk", cashdepositwithcriticalrisk_type, NULL);
    // CashDepositWithSanctioned
    kos_term* cashdepositwithsanctioned_type = kos_mk_prop("CashDepositWithSanctioned");
    kos_ontology_add_type_definition(ontology, "CashDepositWithSanctioned", cashdepositwithsanctioned_type, NULL);
    // CashDepositWithPEP
    kos_term* cashdepositwithpep_type = kos_mk_prop("CashDepositWithPEP");
    kos_ontology_add_type_definition(ontology, "CashDepositWithPEP", cashdepositwithpep_type, NULL);
    // CashDepositWithAdverseMedia
    kos_term* cashdepositwithadversemedia_type = kos_mk_prop("CashDepositWithAdverseMedia");
    kos_ontology_add_type_definition(ontology, "CashDepositWithAdverseMedia", cashdepositwithadversemedia_type, NULL);
    // CashDepositWithSuspiciousActivity
    kos_term* cashdepositwithsuspiciousactivity_type = kos_mk_prop("CashDepositWithSuspiciousActivity");
    kos_ontology_add_type_definition(ontology, "CashDepositWithSuspiciousActivity", cashdepositwithsuspiciousactivity_type, NULL);
    // CashDepositWithTerroristFinancing
    kos_term* cashdepositwithterroristfinancing_type = kos_mk_prop("CashDepositWithTerroristFinancing");
    kos_ontology_add_type_definition(ontology, "CashDepositWithTerroristFinancing", cashdepositwithterroristfinancing_type, NULL);
    // CashDepositWithDrugTrafficking
    kos_term* cashdepositwithdrugtrafficking_type = kos_mk_prop("CashDepositWithDrugTrafficking");
    kos_ontology_add_type_definition(ontology, "CashDepositWithDrugTrafficking", cashdepositwithdrugtrafficking_type, NULL);
    // CashDepositWithHumanTrafficking
    kos_term* cashdepositwithhumantrafficking_type = kos_mk_prop("CashDepositWithHumanTrafficking");
    kos_ontology_add_type_definition(ontology, "CashDepositWithHumanTrafficking", cashdepositwithhumantrafficking_type, NULL);
    // CashDepositWithCorruption
    kos_term* cashdepositwithcorruption_type = kos_mk_prop("CashDepositWithCorruption");
    kos_ontology_add_type_definition(ontology, "CashDepositWithCorruption", cashdepositwithcorruption_type, NULL);
    // CashDepositWithTaxEvasion
    kos_term* cashdepositwithtaxevasion_type = kos_mk_prop("CashDepositWithTaxEvasion");
    kos_ontology_add_type_definition(ontology, "CashDepositWithTaxEvasion", cashdepositwithtaxevasion_type, NULL);
    // CashDepositWithMarketManipulation
    kos_term* cashdepositwithmarketmanipulation_type = kos_mk_prop("CashDepositWithMarketManipulation");
    kos_ontology_add_type_definition(ontology, "CashDepositWithMarketManipulation", cashdepositwithmarketmanipulation_type, NULL);
    // CashDepositWithInsiderTrading
    kos_term* cashdepositwithinsidertrading_type = kos_mk_prop("CashDepositWithInsiderTrading");
    kos_ontology_add_type_definition(ontology, "CashDepositWithInsiderTrading", cashdepositwithinsidertrading_type, NULL);
    // CashWithdrawalWithLowRisk
    kos_term* cashwithdrawalwithlowrisk_type = kos_mk_prop("CashWithdrawalWithLowRisk");
    kos_ontology_add_type_definition(ontology, "CashWithdrawalWithLowRisk", cashwithdrawalwithlowrisk_type, NULL);
    // CashWithdrawalWithMediumRisk
    kos_term* cashwithdrawalwithmediumrisk_type = kos_mk_prop("CashWithdrawalWithMediumRisk");
    kos_ontology_add_type_definition(ontology, "CashWithdrawalWithMediumRisk", cashwithdrawalwithmediumrisk_type, NULL);
    // CashWithdrawalWithHighRisk
    kos_term* cashwithdrawalwithhighrisk_type = kos_mk_prop("CashWithdrawalWithHighRisk");
    kos_ontology_add_type_definition(ontology, "CashWithdrawalWithHighRisk", cashwithdrawalwithhighrisk_type, NULL);
    // CashWithdrawalWithVeryHighRisk
    kos_term* cashwithdrawalwithveryhighrisk_type = kos_mk_prop("CashWithdrawalWithVeryHighRisk");
    kos_ontology_add_type_definition(ontology, "CashWithdrawalWithVeryHighRisk", cashwithdrawalwithveryhighrisk_type, NULL);
    // CashWithdrawalWithCriticalRisk
    kos_term* cashwithdrawalwithcriticalrisk_type = kos_mk_prop("CashWithdrawalWithCriticalRisk");
    kos_ontology_add_type_definition(ontology, "CashWithdrawalWithCriticalRisk", cashwithdrawalwithcriticalrisk_type, NULL);
    // CashWithdrawalWithSanctioned
    kos_term* cashwithdrawalwithsanctioned_type = kos_mk_prop("CashWithdrawalWithSanctioned");
    kos_ontology_add_type_definition(ontology, "CashWithdrawalWithSanctioned", cashwithdrawalwithsanctioned_type, NULL);
    // CashWithdrawalWithPEP
    kos_term* cashwithdrawalwithpep_type = kos_mk_prop("CashWithdrawalWithPEP");
    kos_ontology_add_type_definition(ontology, "CashWithdrawalWithPEP", cashwithdrawalwithpep_type, NULL);
    // CashWithdrawalWithAdverseMedia
    kos_term* cashwithdrawalwithadversemedia_type = kos_mk_prop("CashWithdrawalWithAdverseMedia");
    kos_ontology_add_type_definition(ontology, "CashWithdrawalWithAdverseMedia", cashwithdrawalwithadversemedia_type, NULL);
    // CashWithdrawalWithSuspiciousActivity
    kos_term* cashwithdrawalwithsuspiciousactivity_type = kos_mk_prop("CashWithdrawalWithSuspiciousActivity");
    kos_ontology_add_type_definition(ontology, "CashWithdrawalWithSuspiciousActivity", cashwithdrawalwithsuspiciousactivity_type, NULL);
    // CashWithdrawalWithTerroristFinancing
    kos_term* cashwithdrawalwithterroristfinancing_type = kos_mk_prop("CashWithdrawalWithTerroristFinancing");
    kos_ontology_add_type_definition(ontology, "CashWithdrawalWithTerroristFinancing", cashwithdrawalwithterroristfinancing_type, NULL);
    // CashWithdrawalWithDrugTrafficking
    kos_term* cashwithdrawalwithdrugtrafficking_type = kos_mk_prop("CashWithdrawalWithDrugTrafficking");
    kos_ontology_add_type_definition(ontology, "CashWithdrawalWithDrugTrafficking", cashwithdrawalwithdrugtrafficking_type, NULL);
    // CashWithdrawalWithHumanTrafficking
    kos_term* cashwithdrawalwithhumantrafficking_type = kos_mk_prop("CashWithdrawalWithHumanTrafficking");
    kos_ontology_add_type_definition(ontology, "CashWithdrawalWithHumanTrafficking", cashwithdrawalwithhumantrafficking_type, NULL);
    // CashWithdrawalWithCorruption
    kos_term* cashwithdrawalwithcorruption_type = kos_mk_prop("CashWithdrawalWithCorruption");
    kos_ontology_add_type_definition(ontology, "CashWithdrawalWithCorruption", cashwithdrawalwithcorruption_type, NULL);
    // CashWithdrawalWithTaxEvasion
    kos_term* cashwithdrawalwithtaxevasion_type = kos_mk_prop("CashWithdrawalWithTaxEvasion");
    kos_ontology_add_type_definition(ontology, "CashWithdrawalWithTaxEvasion", cashwithdrawalwithtaxevasion_type, NULL);
    // CashWithdrawalWithMarketManipulation
    kos_term* cashwithdrawalwithmarketmanipulation_type = kos_mk_prop("CashWithdrawalWithMarketManipulation");
    kos_ontology_add_type_definition(ontology, "CashWithdrawalWithMarketManipulation", cashwithdrawalwithmarketmanipulation_type, NULL);
    // CashWithdrawalWithInsiderTrading
    kos_term* cashwithdrawalwithinsidertrading_type = kos_mk_prop("CashWithdrawalWithInsiderTrading");
    kos_ontology_add_type_definition(ontology, "CashWithdrawalWithInsiderTrading", cashwithdrawalwithinsidertrading_type, NULL);
    // CheckDepositWithLowRisk
    kos_term* checkdepositwithlowrisk_type = kos_mk_prop("CheckDepositWithLowRisk");
    kos_ontology_add_type_definition(ontology, "CheckDepositWithLowRisk", checkdepositwithlowrisk_type, NULL);
    // CheckDepositWithMediumRisk
    kos_term* checkdepositwithmediumrisk_type = kos_mk_prop("CheckDepositWithMediumRisk");
    kos_ontology_add_type_definition(ontology, "CheckDepositWithMediumRisk", checkdepositwithmediumrisk_type, NULL);
    // CheckDepositWithHighRisk
    kos_term* checkdepositwithhighrisk_type = kos_mk_prop("CheckDepositWithHighRisk");
    kos_ontology_add_type_definition(ontology, "CheckDepositWithHighRisk", checkdepositwithhighrisk_type, NULL);
    // CheckDepositWithVeryHighRisk
    kos_term* checkdepositwithveryhighrisk_type = kos_mk_prop("CheckDepositWithVeryHighRisk");
    kos_ontology_add_type_definition(ontology, "CheckDepositWithVeryHighRisk", checkdepositwithveryhighrisk_type, NULL);
    // CheckDepositWithCriticalRisk
    kos_term* checkdepositwithcriticalrisk_type = kos_mk_prop("CheckDepositWithCriticalRisk");
    kos_ontology_add_type_definition(ontology, "CheckDepositWithCriticalRisk", checkdepositwithcriticalrisk_type, NULL);
    // CheckDepositWithSanctioned
    kos_term* checkdepositwithsanctioned_type = kos_mk_prop("CheckDepositWithSanctioned");
    kos_ontology_add_type_definition(ontology, "CheckDepositWithSanctioned", checkdepositwithsanctioned_type, NULL);
    // CheckDepositWithPEP
    kos_term* checkdepositwithpep_type = kos_mk_prop("CheckDepositWithPEP");
    kos_ontology_add_type_definition(ontology, "CheckDepositWithPEP", checkdepositwithpep_type, NULL);
    // CheckDepositWithAdverseMedia
    kos_term* checkdepositwithadversemedia_type = kos_mk_prop("CheckDepositWithAdverseMedia");
    kos_ontology_add_type_definition(ontology, "CheckDepositWithAdverseMedia", checkdepositwithadversemedia_type, NULL);
    // CheckDepositWithSuspiciousActivity
    kos_term* checkdepositwithsuspiciousactivity_type = kos_mk_prop("CheckDepositWithSuspiciousActivity");
    kos_ontology_add_type_definition(ontology, "CheckDepositWithSuspiciousActivity", checkdepositwithsuspiciousactivity_type, NULL);
    // CheckDepositWithTerroristFinancing
    kos_term* checkdepositwithterroristfinancing_type = kos_mk_prop("CheckDepositWithTerroristFinancing");
    kos_ontology_add_type_definition(ontology, "CheckDepositWithTerroristFinancing", checkdepositwithterroristfinancing_type, NULL);
    // CheckDepositWithDrugTrafficking
    kos_term* checkdepositwithdrugtrafficking_type = kos_mk_prop("CheckDepositWithDrugTrafficking");
    kos_ontology_add_type_definition(ontology, "CheckDepositWithDrugTrafficking", checkdepositwithdrugtrafficking_type, NULL);
    // CheckDepositWithHumanTrafficking
    kos_term* checkdepositwithhumantrafficking_type = kos_mk_prop("CheckDepositWithHumanTrafficking");
    kos_ontology_add_type_definition(ontology, "CheckDepositWithHumanTrafficking", checkdepositwithhumantrafficking_type, NULL);
    // CheckDepositWithCorruption
    kos_term* checkdepositwithcorruption_type = kos_mk_prop("CheckDepositWithCorruption");
    kos_ontology_add_type_definition(ontology, "CheckDepositWithCorruption", checkdepositwithcorruption_type, NULL);
    // CheckDepositWithTaxEvasion
    kos_term* checkdepositwithtaxevasion_type = kos_mk_prop("CheckDepositWithTaxEvasion");
    kos_ontology_add_type_definition(ontology, "CheckDepositWithTaxEvasion", checkdepositwithtaxevasion_type, NULL);
    // CheckDepositWithMarketManipulation
    kos_term* checkdepositwithmarketmanipulation_type = kos_mk_prop("CheckDepositWithMarketManipulation");
    kos_ontology_add_type_definition(ontology, "CheckDepositWithMarketManipulation", checkdepositwithmarketmanipulation_type, NULL);
    // CheckDepositWithInsiderTrading
    kos_term* checkdepositwithinsidertrading_type = kos_mk_prop("CheckDepositWithInsiderTrading");
    kos_ontology_add_type_definition(ontology, "CheckDepositWithInsiderTrading", checkdepositwithinsidertrading_type, NULL);
    // CheckPaymentWithLowRisk
    kos_term* checkpaymentwithlowrisk_type = kos_mk_prop("CheckPaymentWithLowRisk");
    kos_ontology_add_type_definition(ontology, "CheckPaymentWithLowRisk", checkpaymentwithlowrisk_type, NULL);
    // CheckPaymentWithMediumRisk
    kos_term* checkpaymentwithmediumrisk_type = kos_mk_prop("CheckPaymentWithMediumRisk");
    kos_ontology_add_type_definition(ontology, "CheckPaymentWithMediumRisk", checkpaymentwithmediumrisk_type, NULL);
    // CheckPaymentWithHighRisk
    kos_term* checkpaymentwithhighrisk_type = kos_mk_prop("CheckPaymentWithHighRisk");
    kos_ontology_add_type_definition(ontology, "CheckPaymentWithHighRisk", checkpaymentwithhighrisk_type, NULL);
    // CheckPaymentWithVeryHighRisk
    kos_term* checkpaymentwithveryhighrisk_type = kos_mk_prop("CheckPaymentWithVeryHighRisk");
    kos_ontology_add_type_definition(ontology, "CheckPaymentWithVeryHighRisk", checkpaymentwithveryhighrisk_type, NULL);
    // CheckPaymentWithCriticalRisk
    kos_term* checkpaymentwithcriticalrisk_type = kos_mk_prop("CheckPaymentWithCriticalRisk");
    kos_ontology_add_type_definition(ontology, "CheckPaymentWithCriticalRisk", checkpaymentwithcriticalrisk_type, NULL);
    // CheckPaymentWithSanctioned
    kos_term* checkpaymentwithsanctioned_type = kos_mk_prop("CheckPaymentWithSanctioned");
    kos_ontology_add_type_definition(ontology, "CheckPaymentWithSanctioned", checkpaymentwithsanctioned_type, NULL);
    // CheckPaymentWithPEP
    kos_term* checkpaymentwithpep_type = kos_mk_prop("CheckPaymentWithPEP");
    kos_ontology_add_type_definition(ontology, "CheckPaymentWithPEP", checkpaymentwithpep_type, NULL);
    // CheckPaymentWithAdverseMedia
    kos_term* checkpaymentwithadversemedia_type = kos_mk_prop("CheckPaymentWithAdverseMedia");
    kos_ontology_add_type_definition(ontology, "CheckPaymentWithAdverseMedia", checkpaymentwithadversemedia_type, NULL);
    // CheckPaymentWithSuspiciousActivity
    kos_term* checkpaymentwithsuspiciousactivity_type = kos_mk_prop("CheckPaymentWithSuspiciousActivity");
    kos_ontology_add_type_definition(ontology, "CheckPaymentWithSuspiciousActivity", checkpaymentwithsuspiciousactivity_type, NULL);
    // CheckPaymentWithTerroristFinancing
    kos_term* checkpaymentwithterroristfinancing_type = kos_mk_prop("CheckPaymentWithTerroristFinancing");
    kos_ontology_add_type_definition(ontology, "CheckPaymentWithTerroristFinancing", checkpaymentwithterroristfinancing_type, NULL);
    // CheckPaymentWithDrugTrafficking
    kos_term* checkpaymentwithdrugtrafficking_type = kos_mk_prop("CheckPaymentWithDrugTrafficking");
    kos_ontology_add_type_definition(ontology, "CheckPaymentWithDrugTrafficking", checkpaymentwithdrugtrafficking_type, NULL);
    // CheckPaymentWithHumanTrafficking
    kos_term* checkpaymentwithhumantrafficking_type = kos_mk_prop("CheckPaymentWithHumanTrafficking");
    kos_ontology_add_type_definition(ontology, "CheckPaymentWithHumanTrafficking", checkpaymentwithhumantrafficking_type, NULL);
    // CheckPaymentWithCorruption
    kos_term* checkpaymentwithcorruption_type = kos_mk_prop("CheckPaymentWithCorruption");
    kos_ontology_add_type_definition(ontology, "CheckPaymentWithCorruption", checkpaymentwithcorruption_type, NULL);
    // CheckPaymentWithTaxEvasion
    kos_term* checkpaymentwithtaxevasion_type = kos_mk_prop("CheckPaymentWithTaxEvasion");
    kos_ontology_add_type_definition(ontology, "CheckPaymentWithTaxEvasion", checkpaymentwithtaxevasion_type, NULL);
    // CheckPaymentWithMarketManipulation
    kos_term* checkpaymentwithmarketmanipulation_type = kos_mk_prop("CheckPaymentWithMarketManipulation");
    kos_ontology_add_type_definition(ontology, "CheckPaymentWithMarketManipulation", checkpaymentwithmarketmanipulation_type, NULL);
    // CheckPaymentWithInsiderTrading
    kos_term* checkpaymentwithinsidertrading_type = kos_mk_prop("CheckPaymentWithInsiderTrading");
    kos_ontology_add_type_definition(ontology, "CheckPaymentWithInsiderTrading", checkpaymentwithinsidertrading_type, NULL);
    // CreditCardPaymentWithLowRisk
    kos_term* creditcardpaymentwithlowrisk_type = kos_mk_prop("CreditCardPaymentWithLowRisk");
    kos_ontology_add_type_definition(ontology, "CreditCardPaymentWithLowRisk", creditcardpaymentwithlowrisk_type, NULL);
    // CreditCardPaymentWithMediumRisk
    kos_term* creditcardpaymentwithmediumrisk_type = kos_mk_prop("CreditCardPaymentWithMediumRisk");
    kos_ontology_add_type_definition(ontology, "CreditCardPaymentWithMediumRisk", creditcardpaymentwithmediumrisk_type, NULL);
    // CreditCardPaymentWithHighRisk
    kos_term* creditcardpaymentwithhighrisk_type = kos_mk_prop("CreditCardPaymentWithHighRisk");
    kos_ontology_add_type_definition(ontology, "CreditCardPaymentWithHighRisk", creditcardpaymentwithhighrisk_type, NULL);
    // CreditCardPaymentWithVeryHighRisk
    kos_term* creditcardpaymentwithveryhighrisk_type = kos_mk_prop("CreditCardPaymentWithVeryHighRisk");
    kos_ontology_add_type_definition(ontology, "CreditCardPaymentWithVeryHighRisk", creditcardpaymentwithveryhighrisk_type, NULL);
    // CreditCardPaymentWithCriticalRisk
    kos_term* creditcardpaymentwithcriticalrisk_type = kos_mk_prop("CreditCardPaymentWithCriticalRisk");
    kos_ontology_add_type_definition(ontology, "CreditCardPaymentWithCriticalRisk", creditcardpaymentwithcriticalrisk_type, NULL);
    // CreditCardPaymentWithSanctioned
    kos_term* creditcardpaymentwithsanctioned_type = kos_mk_prop("CreditCardPaymentWithSanctioned");
    kos_ontology_add_type_definition(ontology, "CreditCardPaymentWithSanctioned", creditcardpaymentwithsanctioned_type, NULL);
    // CreditCardPaymentWithPEP
    kos_term* creditcardpaymentwithpep_type = kos_mk_prop("CreditCardPaymentWithPEP");
    kos_ontology_add_type_definition(ontology, "CreditCardPaymentWithPEP", creditcardpaymentwithpep_type, NULL);
    // CreditCardPaymentWithAdverseMedia
    kos_term* creditcardpaymentwithadversemedia_type = kos_mk_prop("CreditCardPaymentWithAdverseMedia");
    kos_ontology_add_type_definition(ontology, "CreditCardPaymentWithAdverseMedia", creditcardpaymentwithadversemedia_type, NULL);
    // CreditCardPaymentWithSuspiciousActivity
    kos_term* creditcardpaymentwithsuspiciousactivity_type = kos_mk_prop("CreditCardPaymentWithSuspiciousActivity");
    kos_ontology_add_type_definition(ontology, "CreditCardPaymentWithSuspiciousActivity", creditcardpaymentwithsuspiciousactivity_type, NULL);
    // CreditCardPaymentWithTerroristFinancing
    kos_term* creditcardpaymentwithterroristfinancing_type = kos_mk_prop("CreditCardPaymentWithTerroristFinancing");
    kos_ontology_add_type_definition(ontology, "CreditCardPaymentWithTerroristFinancing", creditcardpaymentwithterroristfinancing_type, NULL);
    // CreditCardPaymentWithDrugTrafficking
    kos_term* creditcardpaymentwithdrugtrafficking_type = kos_mk_prop("CreditCardPaymentWithDrugTrafficking");
    kos_ontology_add_type_definition(ontology, "CreditCardPaymentWithDrugTrafficking", creditcardpaymentwithdrugtrafficking_type, NULL);
    // CreditCardPaymentWithHumanTrafficking
    kos_term* creditcardpaymentwithhumantrafficking_type = kos_mk_prop("CreditCardPaymentWithHumanTrafficking");
    kos_ontology_add_type_definition(ontology, "CreditCardPaymentWithHumanTrafficking", creditcardpaymentwithhumantrafficking_type, NULL);
    // CreditCardPaymentWithCorruption
    kos_term* creditcardpaymentwithcorruption_type = kos_mk_prop("CreditCardPaymentWithCorruption");
    kos_ontology_add_type_definition(ontology, "CreditCardPaymentWithCorruption", creditcardpaymentwithcorruption_type, NULL);
    // CreditCardPaymentWithTaxEvasion
    kos_term* creditcardpaymentwithtaxevasion_type = kos_mk_prop("CreditCardPaymentWithTaxEvasion");
    kos_ontology_add_type_definition(ontology, "CreditCardPaymentWithTaxEvasion", creditcardpaymentwithtaxevasion_type, NULL);
    // CreditCardPaymentWithMarketManipulation
    kos_term* creditcardpaymentwithmarketmanipulation_type = kos_mk_prop("CreditCardPaymentWithMarketManipulation");
    kos_ontology_add_type_definition(ontology, "CreditCardPaymentWithMarketManipulation", creditcardpaymentwithmarketmanipulation_type, NULL);
    // CreditCardPaymentWithInsiderTrading
    kos_term* creditcardpaymentwithinsidertrading_type = kos_mk_prop("CreditCardPaymentWithInsiderTrading");
    kos_ontology_add_type_definition(ontology, "CreditCardPaymentWithInsiderTrading", creditcardpaymentwithinsidertrading_type, NULL);
    // DebitCardTransactionWithLowRisk
    kos_term* debitcardtransactionwithlowrisk_type = kos_mk_prop("DebitCardTransactionWithLowRisk");
    kos_ontology_add_type_definition(ontology, "DebitCardTransactionWithLowRisk", debitcardtransactionwithlowrisk_type, NULL);
    // DebitCardTransactionWithMediumRisk
    kos_term* debitcardtransactionwithmediumrisk_type = kos_mk_prop("DebitCardTransactionWithMediumRisk");
    kos_ontology_add_type_definition(ontology, "DebitCardTransactionWithMediumRisk", debitcardtransactionwithmediumrisk_type, NULL);
    // DebitCardTransactionWithHighRisk
    kos_term* debitcardtransactionwithhighrisk_type = kos_mk_prop("DebitCardTransactionWithHighRisk");
    kos_ontology_add_type_definition(ontology, "DebitCardTransactionWithHighRisk", debitcardtransactionwithhighrisk_type, NULL);
    // DebitCardTransactionWithVeryHighRisk
    kos_term* debitcardtransactionwithveryhighrisk_type = kos_mk_prop("DebitCardTransactionWithVeryHighRisk");
    kos_ontology_add_type_definition(ontology, "DebitCardTransactionWithVeryHighRisk", debitcardtransactionwithveryhighrisk_type, NULL);
    // DebitCardTransactionWithCriticalRisk
    kos_term* debitcardtransactionwithcriticalrisk_type = kos_mk_prop("DebitCardTransactionWithCriticalRisk");
    kos_ontology_add_type_definition(ontology, "DebitCardTransactionWithCriticalRisk", debitcardtransactionwithcriticalrisk_type, NULL);
    // DebitCardTransactionWithSanctioned
    kos_term* debitcardtransactionwithsanctioned_type = kos_mk_prop("DebitCardTransactionWithSanctioned");
    kos_ontology_add_type_definition(ontology, "DebitCardTransactionWithSanctioned", debitcardtransactionwithsanctioned_type, NULL);
    // DebitCardTransactionWithPEP
    kos_term* debitcardtransactionwithpep_type = kos_mk_prop("DebitCardTransactionWithPEP");
    kos_ontology_add_type_definition(ontology, "DebitCardTransactionWithPEP", debitcardtransactionwithpep_type, NULL);
    // DebitCardTransactionWithAdverseMedia
    kos_term* debitcardtransactionwithadversemedia_type = kos_mk_prop("DebitCardTransactionWithAdverseMedia");
    kos_ontology_add_type_definition(ontology, "DebitCardTransactionWithAdverseMedia", debitcardtransactionwithadversemedia_type, NULL);
    // DebitCardTransactionWithSuspiciousActivity
    kos_term* debitcardtransactionwithsuspiciousactivity_type = kos_mk_prop("DebitCardTransactionWithSuspiciousActivity");
    kos_ontology_add_type_definition(ontology, "DebitCardTransactionWithSuspiciousActivity", debitcardtransactionwithsuspiciousactivity_type, NULL);
    // DebitCardTransactionWithTerroristFinancing
    kos_term* debitcardtransactionwithterroristfinancing_type = kos_mk_prop("DebitCardTransactionWithTerroristFinancing");
    kos_ontology_add_type_definition(ontology, "DebitCardTransactionWithTerroristFinancing", debitcardtransactionwithterroristfinancing_type, NULL);
    // DebitCardTransactionWithDrugTrafficking
    kos_term* debitcardtransactionwithdrugtrafficking_type = kos_mk_prop("DebitCardTransactionWithDrugTrafficking");
    kos_ontology_add_type_definition(ontology, "DebitCardTransactionWithDrugTrafficking", debitcardtransactionwithdrugtrafficking_type, NULL);
    // DebitCardTransactionWithHumanTrafficking
    kos_term* debitcardtransactionwithhumantrafficking_type = kos_mk_prop("DebitCardTransactionWithHumanTrafficking");
    kos_ontology_add_type_definition(ontology, "DebitCardTransactionWithHumanTrafficking", debitcardtransactionwithhumantrafficking_type, NULL);
    // DebitCardTransactionWithCorruption
    kos_term* debitcardtransactionwithcorruption_type = kos_mk_prop("DebitCardTransactionWithCorruption");
    kos_ontology_add_type_definition(ontology, "DebitCardTransactionWithCorruption", debitcardtransactionwithcorruption_type, NULL);
    // DebitCardTransactionWithTaxEvasion
    kos_term* debitcardtransactionwithtaxevasion_type = kos_mk_prop("DebitCardTransactionWithTaxEvasion");
    kos_ontology_add_type_definition(ontology, "DebitCardTransactionWithTaxEvasion", debitcardtransactionwithtaxevasion_type, NULL);
    // DebitCardTransactionWithMarketManipulation
    kos_term* debitcardtransactionwithmarketmanipulation_type = kos_mk_prop("DebitCardTransactionWithMarketManipulation");
    kos_ontology_add_type_definition(ontology, "DebitCardTransactionWithMarketManipulation", debitcardtransactionwithmarketmanipulation_type, NULL);
    // DebitCardTransactionWithInsiderTrading
    kos_term* debitcardtransactionwithinsidertrading_type = kos_mk_prop("DebitCardTransactionWithInsiderTrading");
    kos_ontology_add_type_definition(ontology, "DebitCardTransactionWithInsiderTrading", debitcardtransactionwithinsidertrading_type, NULL);
    // OnlinePaymentWithLowRisk
    kos_term* onlinepaymentwithlowrisk_type = kos_mk_prop("OnlinePaymentWithLowRisk");
    kos_ontology_add_type_definition(ontology, "OnlinePaymentWithLowRisk", onlinepaymentwithlowrisk_type, NULL);
    // OnlinePaymentWithMediumRisk
    kos_term* onlinepaymentwithmediumrisk_type = kos_mk_prop("OnlinePaymentWithMediumRisk");
    kos_ontology_add_type_definition(ontology, "OnlinePaymentWithMediumRisk", onlinepaymentwithmediumrisk_type, NULL);
    // OnlinePaymentWithHighRisk
    kos_term* onlinepaymentwithhighrisk_type = kos_mk_prop("OnlinePaymentWithHighRisk");
    kos_ontology_add_type_definition(ontology, "OnlinePaymentWithHighRisk", onlinepaymentwithhighrisk_type, NULL);
    // OnlinePaymentWithVeryHighRisk
    kos_term* onlinepaymentwithveryhighrisk_type = kos_mk_prop("OnlinePaymentWithVeryHighRisk");
    kos_ontology_add_type_definition(ontology, "OnlinePaymentWithVeryHighRisk", onlinepaymentwithveryhighrisk_type, NULL);
    // OnlinePaymentWithCriticalRisk
    kos_term* onlinepaymentwithcriticalrisk_type = kos_mk_prop("OnlinePaymentWithCriticalRisk");
    kos_ontology_add_type_definition(ontology, "OnlinePaymentWithCriticalRisk", onlinepaymentwithcriticalrisk_type, NULL);
    // OnlinePaymentWithSanctioned
    kos_term* onlinepaymentwithsanctioned_type = kos_mk_prop("OnlinePaymentWithSanctioned");
    kos_ontology_add_type_definition(ontology, "OnlinePaymentWithSanctioned", onlinepaymentwithsanctioned_type, NULL);
    // OnlinePaymentWithPEP
    kos_term* onlinepaymentwithpep_type = kos_mk_prop("OnlinePaymentWithPEP");
    kos_ontology_add_type_definition(ontology, "OnlinePaymentWithPEP", onlinepaymentwithpep_type, NULL);
    // OnlinePaymentWithAdverseMedia
    kos_term* onlinepaymentwithadversemedia_type = kos_mk_prop("OnlinePaymentWithAdverseMedia");
    kos_ontology_add_type_definition(ontology, "OnlinePaymentWithAdverseMedia", onlinepaymentwithadversemedia_type, NULL);
    // OnlinePaymentWithSuspiciousActivity
    kos_term* onlinepaymentwithsuspiciousactivity_type = kos_mk_prop("OnlinePaymentWithSuspiciousActivity");
    kos_ontology_add_type_definition(ontology, "OnlinePaymentWithSuspiciousActivity", onlinepaymentwithsuspiciousactivity_type, NULL);
    // OnlinePaymentWithTerroristFinancing
    kos_term* onlinepaymentwithterroristfinancing_type = kos_mk_prop("OnlinePaymentWithTerroristFinancing");
    kos_ontology_add_type_definition(ontology, "OnlinePaymentWithTerroristFinancing", onlinepaymentwithterroristfinancing_type, NULL);
    // OnlinePaymentWithDrugTrafficking
    kos_term* onlinepaymentwithdrugtrafficking_type = kos_mk_prop("OnlinePaymentWithDrugTrafficking");
    kos_ontology_add_type_definition(ontology, "OnlinePaymentWithDrugTrafficking", onlinepaymentwithdrugtrafficking_type, NULL);
    // OnlinePaymentWithHumanTrafficking
    kos_term* onlinepaymentwithhumantrafficking_type = kos_mk_prop("OnlinePaymentWithHumanTrafficking");
    kos_ontology_add_type_definition(ontology, "OnlinePaymentWithHumanTrafficking", onlinepaymentwithhumantrafficking_type, NULL);
    // OnlinePaymentWithCorruption
    kos_term* onlinepaymentwithcorruption_type = kos_mk_prop("OnlinePaymentWithCorruption");
    kos_ontology_add_type_definition(ontology, "OnlinePaymentWithCorruption", onlinepaymentwithcorruption_type, NULL);
    // OnlinePaymentWithTaxEvasion
    kos_term* onlinepaymentwithtaxevasion_type = kos_mk_prop("OnlinePaymentWithTaxEvasion");
    kos_ontology_add_type_definition(ontology, "OnlinePaymentWithTaxEvasion", onlinepaymentwithtaxevasion_type, NULL);
    // OnlinePaymentWithMarketManipulation
    kos_term* onlinepaymentwithmarketmanipulation_type = kos_mk_prop("OnlinePaymentWithMarketManipulation");
    kos_ontology_add_type_definition(ontology, "OnlinePaymentWithMarketManipulation", onlinepaymentwithmarketmanipulation_type, NULL);
    // OnlinePaymentWithInsiderTrading
    kos_term* onlinepaymentwithinsidertrading_type = kos_mk_prop("OnlinePaymentWithInsiderTrading");
    kos_ontology_add_type_definition(ontology, "OnlinePaymentWithInsiderTrading", onlinepaymentwithinsidertrading_type, NULL);
    // MobilePaymentWithLowRisk
    kos_term* mobilepaymentwithlowrisk_type = kos_mk_prop("MobilePaymentWithLowRisk");
    kos_ontology_add_type_definition(ontology, "MobilePaymentWithLowRisk", mobilepaymentwithlowrisk_type, NULL);
    // MobilePaymentWithMediumRisk
    kos_term* mobilepaymentwithmediumrisk_type = kos_mk_prop("MobilePaymentWithMediumRisk");
    kos_ontology_add_type_definition(ontology, "MobilePaymentWithMediumRisk", mobilepaymentwithmediumrisk_type, NULL);
    // MobilePaymentWithHighRisk
    kos_term* mobilepaymentwithhighrisk_type = kos_mk_prop("MobilePaymentWithHighRisk");
    kos_ontology_add_type_definition(ontology, "MobilePaymentWithHighRisk", mobilepaymentwithhighrisk_type, NULL);
    // MobilePaymentWithVeryHighRisk
    kos_term* mobilepaymentwithveryhighrisk_type = kos_mk_prop("MobilePaymentWithVeryHighRisk");
    kos_ontology_add_type_definition(ontology, "MobilePaymentWithVeryHighRisk", mobilepaymentwithveryhighrisk_type, NULL);
    // MobilePaymentWithCriticalRisk
    kos_term* mobilepaymentwithcriticalrisk_type = kos_mk_prop("MobilePaymentWithCriticalRisk");
    kos_ontology_add_type_definition(ontology, "MobilePaymentWithCriticalRisk", mobilepaymentwithcriticalrisk_type, NULL);
    // MobilePaymentWithSanctioned
    kos_term* mobilepaymentwithsanctioned_type = kos_mk_prop("MobilePaymentWithSanctioned");
    kos_ontology_add_type_definition(ontology, "MobilePaymentWithSanctioned", mobilepaymentwithsanctioned_type, NULL);
    // MobilePaymentWithPEP
    kos_term* mobilepaymentwithpep_type = kos_mk_prop("MobilePaymentWithPEP");
    kos_ontology_add_type_definition(ontology, "MobilePaymentWithPEP", mobilepaymentwithpep_type, NULL);
    // MobilePaymentWithAdverseMedia
    kos_term* mobilepaymentwithadversemedia_type = kos_mk_prop("MobilePaymentWithAdverseMedia");
    kos_ontology_add_type_definition(ontology, "MobilePaymentWithAdverseMedia", mobilepaymentwithadversemedia_type, NULL);
    // MobilePaymentWithSuspiciousActivity
    kos_term* mobilepaymentwithsuspiciousactivity_type = kos_mk_prop("MobilePaymentWithSuspiciousActivity");
    kos_ontology_add_type_definition(ontology, "MobilePaymentWithSuspiciousActivity", mobilepaymentwithsuspiciousactivity_type, NULL);
    // MobilePaymentWithTerroristFinancing
    kos_term* mobilepaymentwithterroristfinancing_type = kos_mk_prop("MobilePaymentWithTerroristFinancing");
    kos_ontology_add_type_definition(ontology, "MobilePaymentWithTerroristFinancing", mobilepaymentwithterroristfinancing_type, NULL);
    // MobilePaymentWithDrugTrafficking
    kos_term* mobilepaymentwithdrugtrafficking_type = kos_mk_prop("MobilePaymentWithDrugTrafficking");
    kos_ontology_add_type_definition(ontology, "MobilePaymentWithDrugTrafficking", mobilepaymentwithdrugtrafficking_type, NULL);
    // MobilePaymentWithHumanTrafficking
    kos_term* mobilepaymentwithhumantrafficking_type = kos_mk_prop("MobilePaymentWithHumanTrafficking");
    kos_ontology_add_type_definition(ontology, "MobilePaymentWithHumanTrafficking", mobilepaymentwithhumantrafficking_type, NULL);
    // MobilePaymentWithCorruption
    kos_term* mobilepaymentwithcorruption_type = kos_mk_prop("MobilePaymentWithCorruption");
    kos_ontology_add_type_definition(ontology, "MobilePaymentWithCorruption", mobilepaymentwithcorruption_type, NULL);
    // MobilePaymentWithTaxEvasion
    kos_term* mobilepaymentwithtaxevasion_type = kos_mk_prop("MobilePaymentWithTaxEvasion");
    kos_ontology_add_type_definition(ontology, "MobilePaymentWithTaxEvasion", mobilepaymentwithtaxevasion_type, NULL);
    // MobilePaymentWithMarketManipulation
    kos_term* mobilepaymentwithmarketmanipulation_type = kos_mk_prop("MobilePaymentWithMarketManipulation");
    kos_ontology_add_type_definition(ontology, "MobilePaymentWithMarketManipulation", mobilepaymentwithmarketmanipulation_type, NULL);
    // MobilePaymentWithInsiderTrading
    kos_term* mobilepaymentwithinsidertrading_type = kos_mk_prop("MobilePaymentWithInsiderTrading");
    kos_ontology_add_type_definition(ontology, "MobilePaymentWithInsiderTrading", mobilepaymentwithinsidertrading_type, NULL);
    // CryptocurrencyTransferWithLowRisk
    kos_term* cryptocurrencytransferwithlowrisk_type = kos_mk_prop("CryptocurrencyTransferWithLowRisk");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransferWithLowRisk", cryptocurrencytransferwithlowrisk_type, NULL);
    // CryptocurrencyTransferWithMediumRisk
    kos_term* cryptocurrencytransferwithmediumrisk_type = kos_mk_prop("CryptocurrencyTransferWithMediumRisk");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransferWithMediumRisk", cryptocurrencytransferwithmediumrisk_type, NULL);
    // CryptocurrencyTransferWithHighRisk
    kos_term* cryptocurrencytransferwithhighrisk_type = kos_mk_prop("CryptocurrencyTransferWithHighRisk");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransferWithHighRisk", cryptocurrencytransferwithhighrisk_type, NULL);
    // CryptocurrencyTransferWithVeryHighRisk
    kos_term* cryptocurrencytransferwithveryhighrisk_type = kos_mk_prop("CryptocurrencyTransferWithVeryHighRisk");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransferWithVeryHighRisk", cryptocurrencytransferwithveryhighrisk_type, NULL);
    // CryptocurrencyTransferWithCriticalRisk
    kos_term* cryptocurrencytransferwithcriticalrisk_type = kos_mk_prop("CryptocurrencyTransferWithCriticalRisk");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransferWithCriticalRisk", cryptocurrencytransferwithcriticalrisk_type, NULL);
    // CryptocurrencyTransferWithSanctioned
    kos_term* cryptocurrencytransferwithsanctioned_type = kos_mk_prop("CryptocurrencyTransferWithSanctioned");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransferWithSanctioned", cryptocurrencytransferwithsanctioned_type, NULL);
    // CryptocurrencyTransferWithPEP
    kos_term* cryptocurrencytransferwithpep_type = kos_mk_prop("CryptocurrencyTransferWithPEP");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransferWithPEP", cryptocurrencytransferwithpep_type, NULL);
    // CryptocurrencyTransferWithAdverseMedia
    kos_term* cryptocurrencytransferwithadversemedia_type = kos_mk_prop("CryptocurrencyTransferWithAdverseMedia");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransferWithAdverseMedia", cryptocurrencytransferwithadversemedia_type, NULL);
    // CryptocurrencyTransferWithSuspiciousActivity
    kos_term* cryptocurrencytransferwithsuspiciousactivity_type = kos_mk_prop("CryptocurrencyTransferWithSuspiciousActivity");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransferWithSuspiciousActivity", cryptocurrencytransferwithsuspiciousactivity_type, NULL);
    // CryptocurrencyTransferWithTerroristFinancing
    kos_term* cryptocurrencytransferwithterroristfinancing_type = kos_mk_prop("CryptocurrencyTransferWithTerroristFinancing");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransferWithTerroristFinancing", cryptocurrencytransferwithterroristfinancing_type, NULL);
    // CryptocurrencyTransferWithDrugTrafficking
    kos_term* cryptocurrencytransferwithdrugtrafficking_type = kos_mk_prop("CryptocurrencyTransferWithDrugTrafficking");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransferWithDrugTrafficking", cryptocurrencytransferwithdrugtrafficking_type, NULL);
    // CryptocurrencyTransferWithHumanTrafficking
    kos_term* cryptocurrencytransferwithhumantrafficking_type = kos_mk_prop("CryptocurrencyTransferWithHumanTrafficking");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransferWithHumanTrafficking", cryptocurrencytransferwithhumantrafficking_type, NULL);
    // CryptocurrencyTransferWithCorruption
    kos_term* cryptocurrencytransferwithcorruption_type = kos_mk_prop("CryptocurrencyTransferWithCorruption");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransferWithCorruption", cryptocurrencytransferwithcorruption_type, NULL);
    // CryptocurrencyTransferWithTaxEvasion
    kos_term* cryptocurrencytransferwithtaxevasion_type = kos_mk_prop("CryptocurrencyTransferWithTaxEvasion");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransferWithTaxEvasion", cryptocurrencytransferwithtaxevasion_type, NULL);
    // CryptocurrencyTransferWithMarketManipulation
    kos_term* cryptocurrencytransferwithmarketmanipulation_type = kos_mk_prop("CryptocurrencyTransferWithMarketManipulation");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransferWithMarketManipulation", cryptocurrencytransferwithmarketmanipulation_type, NULL);
    // CryptocurrencyTransferWithInsiderTrading
    kos_term* cryptocurrencytransferwithinsidertrading_type = kos_mk_prop("CryptocurrencyTransferWithInsiderTrading");
    kos_ontology_add_type_definition(ontology, "CryptocurrencyTransferWithInsiderTrading", cryptocurrencytransferwithinsidertrading_type, NULL);
    // StockTradeWithLowRisk
    kos_term* stocktradewithlowrisk_type = kos_mk_prop("StockTradeWithLowRisk");
    kos_ontology_add_type_definition(ontology, "StockTradeWithLowRisk", stocktradewithlowrisk_type, NULL);
    // StockTradeWithMediumRisk
    kos_term* stocktradewithmediumrisk_type = kos_mk_prop("StockTradeWithMediumRisk");
    kos_ontology_add_type_definition(ontology, "StockTradeWithMediumRisk", stocktradewithmediumrisk_type, NULL);
    // StockTradeWithHighRisk
    kos_term* stocktradewithhighrisk_type = kos_mk_prop("StockTradeWithHighRisk");
    kos_ontology_add_type_definition(ontology, "StockTradeWithHighRisk", stocktradewithhighrisk_type, NULL);
    // StockTradeWithVeryHighRisk
    kos_term* stocktradewithveryhighrisk_type = kos_mk_prop("StockTradeWithVeryHighRisk");
    kos_ontology_add_type_definition(ontology, "StockTradeWithVeryHighRisk", stocktradewithveryhighrisk_type, NULL);
    // StockTradeWithCriticalRisk
    kos_term* stocktradewithcriticalrisk_type = kos_mk_prop("StockTradeWithCriticalRisk");
    kos_ontology_add_type_definition(ontology, "StockTradeWithCriticalRisk", stocktradewithcriticalrisk_type, NULL);
    // StockTradeWithSanctioned
    kos_term* stocktradewithsanctioned_type = kos_mk_prop("StockTradeWithSanctioned");
    kos_ontology_add_type_definition(ontology, "StockTradeWithSanctioned", stocktradewithsanctioned_type, NULL);
    // StockTradeWithPEP
    kos_term* stocktradewithpep_type = kos_mk_prop("StockTradeWithPEP");
    kos_ontology_add_type_definition(ontology, "StockTradeWithPEP", stocktradewithpep_type, NULL);
    // StockTradeWithAdverseMedia
    kos_term* stocktradewithadversemedia_type = kos_mk_prop("StockTradeWithAdverseMedia");
    kos_ontology_add_type_definition(ontology, "StockTradeWithAdverseMedia", stocktradewithadversemedia_type, NULL);
    // StockTradeWithSuspiciousActivity
    kos_term* stocktradewithsuspiciousactivity_type = kos_mk_prop("StockTradeWithSuspiciousActivity");
    kos_ontology_add_type_definition(ontology, "StockTradeWithSuspiciousActivity", stocktradewithsuspiciousactivity_type, NULL);
    // StockTradeWithTerroristFinancing
    kos_term* stocktradewithterroristfinancing_type = kos_mk_prop("StockTradeWithTerroristFinancing");
    kos_ontology_add_type_definition(ontology, "StockTradeWithTerroristFinancing", stocktradewithterroristfinancing_type, NULL);
    // StockTradeWithDrugTrafficking
    kos_term* stocktradewithdrugtrafficking_type = kos_mk_prop("StockTradeWithDrugTrafficking");
    kos_ontology_add_type_definition(ontology, "StockTradeWithDrugTrafficking", stocktradewithdrugtrafficking_type, NULL);
    // StockTradeWithHumanTrafficking
    kos_term* stocktradewithhumantrafficking_type = kos_mk_prop("StockTradeWithHumanTrafficking");
    kos_ontology_add_type_definition(ontology, "StockTradeWithHumanTrafficking", stocktradewithhumantrafficking_type, NULL);
    // StockTradeWithCorruption
    kos_term* stocktradewithcorruption_type = kos_mk_prop("StockTradeWithCorruption");
    kos_ontology_add_type_definition(ontology, "StockTradeWithCorruption", stocktradewithcorruption_type, NULL);
    // StockTradeWithTaxEvasion
    kos_term* stocktradewithtaxevasion_type = kos_mk_prop("StockTradeWithTaxEvasion");
    kos_ontology_add_type_definition(ontology, "StockTradeWithTaxEvasion", stocktradewithtaxevasion_type, NULL);
    // StockTradeWithMarketManipulation
    kos_term* stocktradewithmarketmanipulation_type = kos_mk_prop("StockTradeWithMarketManipulation");
    kos_ontology_add_type_definition(ontology, "StockTradeWithMarketManipulation", stocktradewithmarketmanipulation_type, NULL);
    // StockTradeWithInsiderTrading
    kos_term* stocktradewithinsidertrading_type = kos_mk_prop("StockTradeWithInsiderTrading");
    kos_ontology_add_type_definition(ontology, "StockTradeWithInsiderTrading", stocktradewithinsidertrading_type, NULL);
    // BondTradeWithLowRisk
    kos_term* bondtradewithlowrisk_type = kos_mk_prop("BondTradeWithLowRisk");
    kos_ontology_add_type_definition(ontology, "BondTradeWithLowRisk", bondtradewithlowrisk_type, NULL);
    // BondTradeWithMediumRisk
    kos_term* bondtradewithmediumrisk_type = kos_mk_prop("BondTradeWithMediumRisk");
    kos_ontology_add_type_definition(ontology, "BondTradeWithMediumRisk", bondtradewithmediumrisk_type, NULL);
    // BondTradeWithHighRisk
    kos_term* bondtradewithhighrisk_type = kos_mk_prop("BondTradeWithHighRisk");
    kos_ontology_add_type_definition(ontology, "BondTradeWithHighRisk", bondtradewithhighrisk_type, NULL);
    // BondTradeWithVeryHighRisk
    kos_term* bondtradewithveryhighrisk_type = kos_mk_prop("BondTradeWithVeryHighRisk");
    kos_ontology_add_type_definition(ontology, "BondTradeWithVeryHighRisk", bondtradewithveryhighrisk_type, NULL);
    // BondTradeWithCriticalRisk
    kos_term* bondtradewithcriticalrisk_type = kos_mk_prop("BondTradeWithCriticalRisk");
    kos_ontology_add_type_definition(ontology, "BondTradeWithCriticalRisk", bondtradewithcriticalrisk_type, NULL);
    // BondTradeWithSanctioned
    kos_term* bondtradewithsanctioned_type = kos_mk_prop("BondTradeWithSanctioned");
    kos_ontology_add_type_definition(ontology, "BondTradeWithSanctioned", bondtradewithsanctioned_type, NULL);
    // BondTradeWithPEP
    kos_term* bondtradewithpep_type = kos_mk_prop("BondTradeWithPEP");
    kos_ontology_add_type_definition(ontology, "BondTradeWithPEP", bondtradewithpep_type, NULL);
    // BondTradeWithAdverseMedia
    kos_term* bondtradewithadversemedia_type = kos_mk_prop("BondTradeWithAdverseMedia");
    kos_ontology_add_type_definition(ontology, "BondTradeWithAdverseMedia", bondtradewithadversemedia_type, NULL);
    // BondTradeWithSuspiciousActivity
    kos_term* bondtradewithsuspiciousactivity_type = kos_mk_prop("BondTradeWithSuspiciousActivity");
    kos_ontology_add_type_definition(ontology, "BondTradeWithSuspiciousActivity", bondtradewithsuspiciousactivity_type, NULL);
    // BondTradeWithTerroristFinancing
    kos_term* bondtradewithterroristfinancing_type = kos_mk_prop("BondTradeWithTerroristFinancing");
    kos_ontology_add_type_definition(ontology, "BondTradeWithTerroristFinancing", bondtradewithterroristfinancing_type, NULL);
    // BondTradeWithDrugTrafficking
    kos_term* bondtradewithdrugtrafficking_type = kos_mk_prop("BondTradeWithDrugTrafficking");
    kos_ontology_add_type_definition(ontology, "BondTradeWithDrugTrafficking", bondtradewithdrugtrafficking_type, NULL);
    // BondTradeWithHumanTrafficking
    kos_term* bondtradewithhumantrafficking_type = kos_mk_prop("BondTradeWithHumanTrafficking");
    kos_ontology_add_type_definition(ontology, "BondTradeWithHumanTrafficking", bondtradewithhumantrafficking_type, NULL);
    // BondTradeWithCorruption
    kos_term* bondtradewithcorruption_type = kos_mk_prop("BondTradeWithCorruption");
    kos_ontology_add_type_definition(ontology, "BondTradeWithCorruption", bondtradewithcorruption_type, NULL);
    // BondTradeWithTaxEvasion
    kos_term* bondtradewithtaxevasion_type = kos_mk_prop("BondTradeWithTaxEvasion");
    kos_ontology_add_type_definition(ontology, "BondTradeWithTaxEvasion", bondtradewithtaxevasion_type, NULL);
    // BondTradeWithMarketManipulation
    kos_term* bondtradewithmarketmanipulation_type = kos_mk_prop("BondTradeWithMarketManipulation");
    kos_ontology_add_type_definition(ontology, "BondTradeWithMarketManipulation", bondtradewithmarketmanipulation_type, NULL);
    // BondTradeWithInsiderTrading
    kos_term* bondtradewithinsidertrading_type = kos_mk_prop("BondTradeWithInsiderTrading");
    kos_ontology_add_type_definition(ontology, "BondTradeWithInsiderTrading", bondtradewithinsidertrading_type, NULL);
    // DerivativeTradeWithLowRisk
    kos_term* derivativetradewithlowrisk_type = kos_mk_prop("DerivativeTradeWithLowRisk");
    kos_ontology_add_type_definition(ontology, "DerivativeTradeWithLowRisk", derivativetradewithlowrisk_type, NULL);
    // DerivativeTradeWithMediumRisk
    kos_term* derivativetradewithmediumrisk_type = kos_mk_prop("DerivativeTradeWithMediumRisk");
    kos_ontology_add_type_definition(ontology, "DerivativeTradeWithMediumRisk", derivativetradewithmediumrisk_type, NULL);
    // DerivativeTradeWithHighRisk
    kos_term* derivativetradewithhighrisk_type = kos_mk_prop("DerivativeTradeWithHighRisk");
    kos_ontology_add_type_definition(ontology, "DerivativeTradeWithHighRisk", derivativetradewithhighrisk_type, NULL);
    // DerivativeTradeWithVeryHighRisk
    kos_term* derivativetradewithveryhighrisk_type = kos_mk_prop("DerivativeTradeWithVeryHighRisk");
    kos_ontology_add_type_definition(ontology, "DerivativeTradeWithVeryHighRisk", derivativetradewithveryhighrisk_type, NULL);
    // DerivativeTradeWithCriticalRisk
    kos_term* derivativetradewithcriticalrisk_type = kos_mk_prop("DerivativeTradeWithCriticalRisk");
    kos_ontology_add_type_definition(ontology, "DerivativeTradeWithCriticalRisk", derivativetradewithcriticalrisk_type, NULL);
    // DerivativeTradeWithSanctioned
    kos_term* derivativetradewithsanctioned_type = kos_mk_prop("DerivativeTradeWithSanctioned");
    kos_ontology_add_type_definition(ontology, "DerivativeTradeWithSanctioned", derivativetradewithsanctioned_type, NULL);
    // DerivativeTradeWithPEP
    kos_term* derivativetradewithpep_type = kos_mk_prop("DerivativeTradeWithPEP");
    kos_ontology_add_type_definition(ontology, "DerivativeTradeWithPEP", derivativetradewithpep_type, NULL);
    // DerivativeTradeWithAdverseMedia
    kos_term* derivativetradewithadversemedia_type = kos_mk_prop("DerivativeTradeWithAdverseMedia");
    kos_ontology_add_type_definition(ontology, "DerivativeTradeWithAdverseMedia", derivativetradewithadversemedia_type, NULL);
    // DerivativeTradeWithSuspiciousActivity
    kos_term* derivativetradewithsuspiciousactivity_type = kos_mk_prop("DerivativeTradeWithSuspiciousActivity");
    kos_ontology_add_type_definition(ontology, "DerivativeTradeWithSuspiciousActivity", derivativetradewithsuspiciousactivity_type, NULL);
    // DerivativeTradeWithTerroristFinancing
    kos_term* derivativetradewithterroristfinancing_type = kos_mk_prop("DerivativeTradeWithTerroristFinancing");
    kos_ontology_add_type_definition(ontology, "DerivativeTradeWithTerroristFinancing", derivativetradewithterroristfinancing_type, NULL);
    // DerivativeTradeWithDrugTrafficking
    kos_term* derivativetradewithdrugtrafficking_type = kos_mk_prop("DerivativeTradeWithDrugTrafficking");
    kos_ontology_add_type_definition(ontology, "DerivativeTradeWithDrugTrafficking", derivativetradewithdrugtrafficking_type, NULL);
    // DerivativeTradeWithHumanTrafficking
    kos_term* derivativetradewithhumantrafficking_type = kos_mk_prop("DerivativeTradeWithHumanTrafficking");
    kos_ontology_add_type_definition(ontology, "DerivativeTradeWithHumanTrafficking", derivativetradewithhumantrafficking_type, NULL);
    // DerivativeTradeWithCorruption
    kos_term* derivativetradewithcorruption_type = kos_mk_prop("DerivativeTradeWithCorruption");
    kos_ontology_add_type_definition(ontology, "DerivativeTradeWithCorruption", derivativetradewithcorruption_type, NULL);
    // DerivativeTradeWithTaxEvasion
    kos_term* derivativetradewithtaxevasion_type = kos_mk_prop("DerivativeTradeWithTaxEvasion");
    kos_ontology_add_type_definition(ontology, "DerivativeTradeWithTaxEvasion", derivativetradewithtaxevasion_type, NULL);
    // DerivativeTradeWithMarketManipulation
    kos_term* derivativetradewithmarketmanipulation_type = kos_mk_prop("DerivativeTradeWithMarketManipulation");
    kos_ontology_add_type_definition(ontology, "DerivativeTradeWithMarketManipulation", derivativetradewithmarketmanipulation_type, NULL);
    // DerivativeTradeWithInsiderTrading
    kos_term* derivativetradewithinsidertrading_type = kos_mk_prop("DerivativeTradeWithInsiderTrading");
    kos_ontology_add_type_definition(ontology, "DerivativeTradeWithInsiderTrading", derivativetradewithinsidertrading_type, NULL);

    printf("[FinanceGenerated] ✓ Added 1131 generated types\n");
}
