#!/usr/bin/env python3
# tools/generate_finance_types.py
# 自动生成金融领域类型定义的Python脚本
# 生成数千个金融领域常见类型

import os

# 金融领域类型定义模板（扩展版，生成数千个类型）
FINANCE_TYPES = {
    # 账户类型（扩展）
    "account_types": [
        "SavingsAccount", "CheckingAccount", "CurrentAccount", "DepositAccount",
        "InvestmentAccount", "TradingAccount", "MarginAccount", "ForexAccount",
        "CryptocurrencyAccount", "OffshoreAccount", "TrustAccount", "EscrowAccount",
        "CorporateAccount", "PersonalAccount", "JointAccount", "CustodialAccount",
        "RetirementAccount", "PensionAccount", "IRA", "401K", "RothIRA", "SEPIRA",
        "HealthSavingsAccount", "EducationSavingsAccount", "CustodianAccount",
        "NomineeAccount", "BeneficialOwnerAccount", "ShellCompanyAccount",
        "FoundationAccount", "CharityAccount", "EndowmentAccount"
    ],
    
    # 交易类型（扩展）
    "transaction_types": [
        "WireTransfer", "ACHTransfer", "SWIFTTransfer", "CashDeposit",
        "CashWithdrawal", "CheckDeposit", "CheckPayment", "CreditCardPayment",
        "DebitCardTransaction", "OnlinePayment", "MobilePayment", "CryptocurrencyTransfer",
        "StockTrade", "BondTrade", "DerivativeTrade", "ForexTrade",
        "CommodityTrade", "OptionsTrade", "FuturesTrade", "SwapTransaction",
        "LoanDisbursement", "LoanRepayment", "InterestPayment", "DividendPayment",
        "InsurancePremium", "InsuranceClaim", "PensionPayment", "SalaryPayment",
        "TaxPayment", "RefundPayment", "GiftTransfer", "CharityDonation",
        "RealEstatePurchase", "RealEstateSale", "ArtPurchase", "ArtSale",
        "JewelryPurchase", "JewelrySale", "VehiclePurchase", "VehicleSale",
        "CasinoDeposit", "CasinoWithdrawal", "GamingTransaction", "LotteryPayment",
        "HawalaTransfer", "UndergroundBankingTransfer", "MoneyOrder", "TravelersCheck",
        "PrepaidCardLoad", "PrepaidCardSpend", "CryptocurrencyExchange", "TokenTransfer"
    ],
    
    # 金融机构类型（扩展）
    "institution_types": [
        "CommercialBank", "InvestmentBank", "CentralBank", "CreditUnion",
        "SavingsBank", "PrivateBank", "OffshoreBank", "ShadowBank",
        "BrokerageFirm", "SecuritiesFirm", "InsuranceCompany", "PensionFund",
        "HedgeFund", "PrivateEquityFund", "MutualFund", "ETF",
        "PaymentProcessor", "MoneyServiceBusiness", "CryptocurrencyExchange",
        "PeerToPeerLender", "CrowdfundingPlatform", "FintechCompany",
        "MicrofinanceInstitution", "DevelopmentBank", "ExportImportBank",
        "MortgageBank", "ConsumerFinanceCompany", "AutoFinanceCompany",
        "FactoringCompany", "LeasingCompany", "AssetManagementCompany",
        "WealthManagementCompany", "FamilyOffice", "TrustCompany"
    ],
    
    # 国家/地区（扩展至100+）
    "countries": [
        "UnitedStates", "UnitedKingdom", "Switzerland", "Singapore",
        "HongKong", "Luxembourg", "CaymanIslands", "BritishVirginIslands",
        "Bermuda", "Panama", "Bahamas", "Jersey", "Guernsey", "IsleOfMan",
        "Cyprus", "Malta", "Ireland", "Netherlands", "Belgium", "Germany",
        "France", "Italy", "Spain", "Portugal", "Greece", "Austria",
        "Sweden", "Norway", "Denmark", "Finland", "Iceland", "Poland",
        "CzechRepublic", "Hungary", "Romania", "Bulgaria", "Croatia",
        "Japan", "China", "SouthKorea", "India", "Australia", "NewZealand",
        "Canada", "Mexico", "Brazil", "Argentina", "Chile", "Colombia",
        "SouthAfrica", "Egypt", "Nigeria", "Kenya", "UAE", "SaudiArabia",
        "Israel", "Turkey", "Russia", "Ukraine", "Kazakhstan",
        "Indonesia", "Malaysia", "Thailand", "Vietnam", "Philippines",
        "Bangladesh", "Pakistan", "SriLanka", "Myanmar", "Cambodia",
        "Laos", "Mongolia", "Nepal", "Bhutan", "Afghanistan",
        "Iran", "Iraq", "Jordan", "Lebanon", "Syria",
        "Yemen", "Oman", "Qatar", "Kuwait", "Bahrain",
        "Morocco", "Algeria", "Tunisia", "Libya", "Sudan",
        "Ethiopia", "Tanzania", "Uganda", "Ghana", "IvoryCoast",
        "Senegal", "Angola", "Mozambique", "Zimbabwe", "Botswana",
        "Namibia", "Mauritius", "Seychelles", "Maldives", "Fiji"
    ],
    
    # 货币类型（扩展）
    "currencies": [
        "USD", "EUR", "GBP", "JPY", "CHF", "CNY", "HKD", "SGD",
        "AUD", "CAD", "NZD", "SEK", "NOK", "DKK", "PLN", "CZK",
        "HUF", "RUB", "INR", "KRW", "BRL", "MXN", "ZAR", "TRY",
        "AED", "SAR", "ILS", "THB", "MYR", "IDR", "PHP", "VND",
        "BTC", "ETH", "USDT", "USDC", "BNB", "XRP", "ADA", "SOL",
        "DOGE", "DOT", "MATIC", "AVAX", "LINK", "UNI", "LTC", "BCH",
        "XLM", "ATOM", "ALGO", "VET", "ICP", "FIL", "TRX", "ETC"
    ],
    
    # 交易状态（扩展）
    "transaction_statuses": [
        "Pending", "Processing", "Completed", "Failed", "Cancelled",
        "Reversed", "Refunded", "Suspended", "Frozen", "Blocked",
        "UnderReview", "RequiresVerification", "Flagged", "Rejected",
        "Approved", "Declined", "Timeout", "Expired", "Invalid",
        "Duplicate", "Fraudulent", "Chargeback", "Disputed", "Resolved"
    ],
    
    # 风险等级（扩展）
    "risk_levels": [
        "LowRisk", "MediumRisk", "HighRisk", "VeryHighRisk", "CriticalRisk",
        "Sanctioned", "PEP", "AdverseMedia", "SuspiciousActivity",
        "TerroristFinancing", "DrugTrafficking", "HumanTrafficking",
        "Corruption", "TaxEvasion", "MarketManipulation", "InsiderTrading"
    ],
    
    # 合规类型（扩展）
    "compliance_types": [
        "KYC", "AML", "CTF", "SanctionsScreening", "PEPCheck",
        "AdverseMediaCheck", "TransactionMonitoring", "SuspiciousActivityReport",
        "CurrencyTransactionReport", "ForeignAccountTaxCompliance", "CRS",
        "FATCA", "GDPR", "PCI", "SOX", "MiFID", "EMIR", "DoddFrank",
        "BaselIII", "SolvencyII", "PSD2", "GDPR", "CCPA"
    ],
    
    # 洗钱手法（扩展）
    "money_laundering_methods": [
        "Structuring", "Smurfing", "Layering", "Integration", "TradeBased",
        "ShellCompany", "NomineeAccount", "OffshoreStructure", "CryptocurrencyMixer",
        "Hawala", "UndergroundBanking", "CasinoLaundering", "RealEstateLaundering",
        "ArtLaundering", "JewelryLaundering", "VirtualAssetLaundering",
        "TradeBasedLaundering", "InvoiceFraud", "OverInvoicing", "UnderInvoicing",
        "FalseInvoicing", "RoundTripping", "LoanBack", "GiftLaundering"
    ],
    
    # 可疑活动类型（扩展）
    "suspicious_activities": [
        "UnusualTransactionPattern", "RapidMovement", "CircularTransactions",
        "StructuredTransactions", "HighValueWithoutPurpose", "CrossBorderNoBusiness",
        "MultipleAccountsSameBeneficiary", "FrequentSmallDeposits", "LargeCashTransactions",
        "TransactionsWithSanctionedCountries", "TransactionsWithPEP", "ComplexTransactionChain",
        "TransactionsInconsistentWithProfile", "RapidAccountOpeningClosing", "UnexplainedWealth",
        "RoundTripTransactions", "BackToBackLoans", "FalseIdentity", "IdentityTheft",
        "AccountTakeover", "SyntheticIdentity", "MuleAccount", "FunnelAccount",
        "SmurfingPattern", "StructuringPattern", "LayeringPattern", "IntegrationPattern"
    ],
    
    # 监管机构（扩展）
    "regulatory_bodies": [
        "FINCEN", "SEC", "CFTC", "OCC", "FDIC", "FRB", "FCA", "PRA",
        "ECB", "ESMA", "BaFin", "AMF", "CONSOB", "CNMV", "AFM", "FINMA",
        "MAS", "HKMA", "ASIC", "OSFI", "CNBV", "CVM", "CBRF",
        "FSA", "FSC", "SFC", "ASIC", "FMA", "FSCL", "FSP", "FMA"
    ],
    
    # 文档类型（扩展）
    "document_types": [
        "Passport", "NationalID", "DriversLicense", "BirthCertificate",
        "ProofOfAddress", "BankStatement", "TaxReturn", "EmploymentLetter",
        "BusinessLicense", "CertificateOfIncorporation", "ArticlesOfAssociation",
        "ShareCertificate", "PowerOfAttorney", "TrustDeed", "BeneficialOwnership",
        "UtilityBill", "RentalAgreement", "MortgageStatement", "CreditReport",
        "FinancialStatement", "AuditReport", "ComplianceCertificate", "KYCForm"
    ],
    
    # 职业类型（扩展）
    "occupations": [
        "Banker", "Trader", "Broker", "Accountant", "Lawyer", "Consultant",
        "RealEstateAgent", "JewelryDealer", "ArtDealer", "CasinoOperator",
        "MoneyServiceOperator", "CryptocurrencyExchanger", "Politician", "GovernmentOfficial",
        "Diplomat", "MilitaryOfficer", "Judge", "Prosecutor", "PoliceOfficer",
        "Doctor", "Engineer", "Architect", "Teacher", "Professor", "Researcher",
        "CEO", "CFO", "CTO", "Director", "Manager", "Executive", "Entrepreneur"
    ],
    
    # 行业类型（扩展）
    "industries": [
        "Banking", "Securities", "Insurance", "RealEstate", "Construction",
        "Manufacturing", "Retail", "Wholesale", "ImportExport", "Tourism",
        "Gaming", "Entertainment", "Media", "Technology", "Healthcare",
        "Education", "NonProfit", "ReligiousOrganization", "Charity",
        "Agriculture", "Mining", "Energy", "Transportation", "Logistics",
        "Telecommunications", "Utilities", "FoodService", "Hospitality"
    ],
    
    # 新增：交易金额范围
    "amount_ranges": [
        "MicroTransaction", "SmallTransaction", "MediumTransaction", "LargeTransaction",
        "VeryLargeTransaction", "MegaTransaction", "BelowReportingThreshold",
        "AboveReportingThreshold", "StructuredAmount", "RoundAmount"
    ],
    
    # 新增：交易频率
    "transaction_frequencies": [
        "OneTime", "Occasional", "Regular", "Frequent", "VeryFrequent",
        "Daily", "Weekly", "Monthly", "Quarterly", "Yearly",
        "Irregular", "SuspiciousFrequency"
    ],
    
    # 新增：账户关系
    "account_relationships": [
        "SameOwner", "RelatedOwner", "FamilyMember", "BusinessPartner",
        "Subsidiary", "ParentCompany", "Affiliate", "Nominee",
        "BeneficialOwner", "Trustee", "Beneficiary", "PowerOfAttorney",
        "AuthorizedSigner", "JointOwner", "NoRelationship"
    ],
    
    # 新增：交易目的
    "transaction_purposes": [
        "Salary", "BusinessPayment", "Investment", "Loan", "Gift",
        "Charity", "TaxPayment", "Purchase", "Sale", "Refund",
        "Unknown", "Suspicious", "NoPurpose", "MultiplePurposes"
    ],
    
    # 新增：账户状态
    "account_statuses": [
        "Active", "Inactive", "Dormant", "Closed", "Frozen",
        "Blocked", "UnderInvestigation", "Suspended", "Restricted"
    ],
    
    # 新增：客户类型
    "customer_types": [
        "Individual", "Corporate", "Partnership", "Trust", "Foundation",
        "Government", "NGO", "Charity", "ReligiousOrganization"
    ],
    
    # 新增：账户开立渠道
    "account_channels": [
        "Branch", "Online", "Mobile", "ATM", "Phone",
        "Mail", "Agent", "ThirdParty", "Unknown"
    ],
    
    # 新增：交易渠道
    "transaction_channels": [
        "Branch", "ATM", "Online", "Mobile", "Phone",
        "Mail", "SWIFT", "ACH", "Card", "Cryptocurrency"
    ],
    
    # 新增：地理位置类型
    "location_types": [
        "Domestic", "CrossBorder", "Offshore", "TaxHaven",
        "HighRiskJurisdiction", "SanctionedCountry", "FATFListed"
    ],
    
    # 新增：时间模式
    "time_patterns": [
        "BusinessHours", "AfterHours", "Weekend", "Holiday",
        "RapidSequence", "SuspiciousTiming", "UnusualTime"
    ]
}

def sanitize_variable_name(name):
    """将类型名称转换为有效的C变量名"""
    # 如果以数字开头，添加前缀
    if name and name[0].isdigit():
        return "type_" + name.lower().replace("-", "_").replace(" ", "_")
    return name.lower().replace("-", "_").replace(" ", "_")

def generate_type_definition_c(name, base_type="Prop"):
    """生成单个类型定义的C代码"""
    var_name = sanitize_variable_name(name)
    return f'''    // {name}
    kos_term* {var_name}_type = kos_mk_prop("{name}");
    kos_ontology_add_type_definition(ontology, "{name}", {var_name}_type, NULL);
'''

def generate_event_type_definition_c(name, fields):
    """生成事件类型定义的C代码（Σ类型）"""
    code = f'''    // {name}: 事件类型
    // {name} = '''
    
    sigma_parts = []
    for field_name, field_type in fields:
        sigma_parts.append(f"Σ({field_name}: {field_type})")
    sigma_parts.append("Prop")
    code += " ".join(sigma_parts) + "\n"
    
    # 构建嵌套的Σ类型
    code += f"    kos_term* {name.lower()}_prop = kos_mk_prop(\"Prop\");\n"
    
    # 从最后一个字段开始构建
    for i in range(len(fields) - 1, -1, -1):
        field_name, field_type = fields[i]
        field_type_var = field_type.lower().replace("ID", "_id").replace("Type", "_type")
        if i == len(fields) - 1:
            code += f"    kos_term* {name.lower()}_sigma_{i} = kos_mk_sigma(\n"
            code += f"        kos_ontology_find_type_definition(ontology, \"{field_type}\"),\n"
            code += f"        {name.lower()}_prop);\n"
        else:
            code += f"    kos_term* {name.lower()}_sigma_{i} = kos_mk_sigma(\n"
            code += f"        kos_ontology_find_type_definition(ontology, \"{field_type}\"),\n"
            code += f"        {name.lower()}_sigma_{i+1});\n"
    
    code += f"    kos_ontology_add_type_definition(ontology, \"{name}\", {name.lower()}_sigma_0, NULL);\n"
    return code

def generate_c_code():
    """生成完整的C代码文件"""
    output_file = "src/domain/finance/ontology_extended_generated.c"
    
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write("""// src/domain/finance/ontology_extended_generated.c
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
    
    printf("[FinanceGenerated] Adding generated finance types...\\n");
    
""")
        
        type_count = 0
        seen_types = set()  # 用于跟踪已添加的类型，避免重复
        
        # 生成基础类型
        for category, types in FINANCE_TYPES.items():
            f.write(f"    // ========== {category} ==========\\n")
            for type_name in types:
                # 检查是否已添加（避免重复）
                if type_name not in seen_types:
                    f.write(generate_type_definition_c(type_name))
                    seen_types.add(type_name)
                    type_count += 1
                else:
                    f.write(f"    // {type_name} (already added, skipping duplicate)\\n")
        
        # 生成复合事件类型
        f.write("\n    // ========== 复合事件类型 ==========\n")
        
        # 生成各种交易事件类型（全部）
        transaction_types = FINANCE_TYPES["transaction_types"]
        for tx_type in transaction_types:
            event_type_name = f"{tx_type}Event"
            if event_type_name not in seen_types:
                fields = [
                    ("tx_id", "TransactionID"),
                    ("from_account", "AccountID"),
                    ("to_account", "AccountID"),
                    ("amount", "Amount"),
                    ("currency", "Currency"),
                    ("timestamp", "Time")
                ]
                f.write(generate_event_type_definition_c(event_type_name, fields))
                seen_types.add(event_type_name)
                type_count += 1
        
        # 生成可疑活动事件类型（全部）
        suspicious_activities = FINANCE_TYPES["suspicious_activities"]
        for activity in suspicious_activities:
            event_type_name = f"{activity}Event"
            if event_type_name not in seen_types:
                fields = [
                    ("case_id", "TransactionID"),
                    ("account_id", "AccountID"),
                    ("description", "Prop"),
                    ("timestamp", "Time")
                ]
                f.write(generate_event_type_definition_c(event_type_name, fields))
                seen_types.add(event_type_name)
                type_count += 1
        
        # 生成洗钱相关事件类型（全部）
        ml_methods = FINANCE_TYPES["money_laundering_methods"]
        for method in ml_methods:
            suspicion_type_name = f"{method}Suspicion"
            if suspicion_type_name not in seen_types:
                fields = [
                    ("case_id", "TransactionID"),
                    ("account_id", "AccountID"),
                    ("evidence", "Prop"),
                    ("risk_level", "Prop"),
                    ("timestamp", "Time")
                ]
                f.write(generate_event_type_definition_c(suspicion_type_name, fields))
                seen_types.add(suspicion_type_name)
                type_count += 1
        
        # 生成账户类型组合（账户类型 × 国家）
        f.write("\n    // ========== 账户类型组合 ==========\n")
        account_types = FINANCE_TYPES["account_types"][:10]  # 取前10个
        countries = FINANCE_TYPES["countries"][:20]  # 取前20个国家
        for acc_type in account_types:
            for country in countries:
                type_name = f"{acc_type}In{country}"
                if type_name not in seen_types:
                    f.write(generate_type_definition_c(type_name))
                    seen_types.add(type_name)
                    type_count += 1
        
        # 生成交易模式类型（交易类型 × 风险等级）
        f.write("\n    // ========== 交易模式类型 ==========\n")
        tx_types = FINANCE_TYPES["transaction_types"][:15]  # 取前15个
        risk_levels = FINANCE_TYPES["risk_levels"]
        for tx_type in tx_types:
            for risk in risk_levels:
                type_name = f"{tx_type}With{risk}"
                if type_name not in seen_types:
                    f.write(generate_type_definition_c(type_name))
                    seen_types.add(type_name)
                    type_count += 1
        
        f.write(f"""
    printf("[FinanceGenerated] ✓ Added {type_count} generated types\\n");
}}
""")
    
    print(f"Generated {output_file} with {type_count} types")
    return type_count

if __name__ == "__main__":
    count = generate_c_code()
    print(f"Total types generated: {count}")

