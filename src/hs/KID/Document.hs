{-| Document (Annex I)

    Document generation with LaTeX
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module KID.Document (
    generateDocument
  , Language(..)
  ) where

import qualified Data.ByteString.Lazy.Char8   as BS
import           Data.Either
import           KID.Domain
import           System.Process               as P
import           Text.LaTeX
import           Text.LaTeX.Base.Syntax
import           Text.LaTeX.Packages.Bigstrut
import           Text.LaTeX.Packages.Fancyhdr
import           Text.LaTeX.Packages.Geometry
import           Text.LaTeX.Packages.Hyperref
import           Text.LaTeX.Packages.Inputenc
import           Text.LaTeX.Packages.Multirow
import           Text.LaTeX.Packages.TikZ

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- | Generate the document
-- The default language is EN
generateDocument :: Language -> (Contract, RiskSummary) -> IO BS.ByteString
generateDocument l (c,r) = execLaTeXT (kid l (c,r)) >>= renderFile "kid.tex"
  >> readProcess "pdflatex" ["kid.tex"] [] -- TODO: move to MimeTypes.hs
  >> BS.readFile "kid.pdf"

-- | Supported languages
data Language =
    DE
  | FR
  | IT
  | EN
  deriving (Read, Show, Enum, Bounded)

kid :: Monad m => Language -> (Contract, RiskSummary) -> LaTeXT_ m
kid l (c,r) = preamble l >> document (body l (c,r))

-- | Document structure
settings :: Language -> HdrSettings
settings l = defaultHdrSettings { leftHeader = title l , headRuleWidth = Pt 2 }
  where
    title DE = "Basisinformationsblatt"
    title FR = "Key Information Document" -- TODO
    title IT = "Key Information Document" -- TODO
    title EN = "Key Information Document"

preamble :: Monad m => Language -> LaTeXT_ m
preamble l = do
  documentclass [] article -- Probably use custom documentclass
  usepackage [utf8] inputenc
  usepackage [] tikz
  usepackage [] multirowp
  usepackage [] bigstrutp
  usepackage [] geometry
  usepackage [] hyperref
  usepackage [] fancyhdr
  applyGeometry [GWidth $ Cm 19] -- TODO: to custom documentclass
  applyHdrSettings $ settings l

body :: Monad m => Language -> (Contract, RiskSummary) -> LaTeXT_ m
body l (c,r) = do
  section_purpose l
  section_product l c
  section_what_is_this_product l c
  section_risk r
  section_performance_scenarios l c r
  section_costs l c
  section_how_can_i_complain l c
  section_other_relevant_information l c

---------------------------------------------------------------------------------------------------
-- | Purpose
---------------------------------------------------------------------------------------------------

section_purpose :: Monad m => Language -> LaTeXT_ m
section_purpose DE = do
  section' "Zweck"
  "Dieses Dokument liefert Ihnen wichtige Informationen zu diesem Anlageprodukt. Es ist kein Marketingmaterial. Die Informationen sind gesetzlich vorgeschrieben, um Ihnen zu helfen, die Art, Risiken, Kosten, potenzielle Gewinne und Verluste dieses Produkts zu verstehen und es mit anderen Produkten zu vergleichen."

section_purpose FR = do
  section' "Objet"
  "Ce document vous fournit des informations clés sur ce produit d'investissement. Ce n'est pas du matériel de marketing. Les informations sont requises par la loi pour vous aider à comprendre la nature, les risques, les coûts, les gains et les pertes potentiels de ce produit et pour vous aider à le comparer avec d'autres produits."

section_purpose IT = do
  section' "Scopo"
  "Questo documento fornisce informazioni chiave su questo prodotto d'investimento. Non si tratta di materiale di marketing. Le informazioni sono richieste per legge per aiutarti a comprendere la natura, i rischi, i costi, i potenziali guadagni e perdite di questo prodotto e per aiutarti a confrontarlo con altri prodotti."

section_purpose EN = do
  section' "Purpose"
  "This document provides you with key information about this investment product. It is not marketing material. The information is required by law to help you understand the nature, risks, costs, potential gains and losses of this product and to help you compare it with other products."

---------------------------------------------------------------------------------------------------
-- | Product
---------------------------------------------------------------------------------------------------

section_product :: Monad m => Language -> Contract -> LaTeXT_ m
section_product DE c = section' "Produkt" -- TODO
section_product FR c = section' "Produit" -- TODO
section_product IT c = section' "Prodoto" -- TODO
section_product EN Contract{..} = do
  section' "Product"

  textbf "Name: "                <> fromString product_name        <> newline
  textbf "Product Type: "        <> fromString (show product_type) <> newline
  textbf "Competent Authority: " <> "-"                            <> newline

---------------------------------------------------------------------------------------------------
-- | What is this product?
---------------------------------------------------------------------------------------------------

section_what_is_this_product :: Monad m => Language -> Contract -> LaTeXT_ m
section_what_is_this_product DE c = do

  section' "Um welche Art von Produkt handelt es sich?"

  subsection' "Art"
  subsection' "Ziele"
  subsection' "Zielgruppe Kleinanleger"

section_what_is_this_product FR c = section' "En quoi consiste ce produit?" -- TODO
section_what_is_this_product IT c = section' "Cos’è questo prodotto?" -- TODO

section_what_is_this_product EN c = do

  section' "What is this product?"

  subsection' $ textbf "Type"
  "Debt instrument in bearer form, evidenced in the form of uncertificated book-entry securities, governed by Swiss law."

  subsection' $ textbf "Objectives"
  "The objective of this product is to provide you with the possibility of receiving an enhanced return in exchange for taking the risk of a loss of some or all of your investment. The product is a complex financial instrument linked to 3 Underlyings. It has a fixed term and will be due on the Maturity Date, unless terminated early."

  subsection' $ textbf "Intended retail investor"

---------------------------------------------------------------------------------------------------
-- | What are the risks and what could I get in return
---------------------------------------------------------------------------------------------------

section_risk :: Monad m => RiskSummary -> LaTeXT_ m
section_risk r = section' "RiskSummary" >> section_sri r >> section_risk_body r

section_sri :: Monad m => RiskSummary -> LaTeXT_ m
section_sri RiskSummary{..} = center $ do
  raw "\\tikzstyle{int}=[draw, minimum size=2em]"
  raw "\\tikzstyle{che}=[draw, minimum size=2em, fill=gray]"
  raw "\\begin{figure}[!h]"
  raw "\\centering"
  raw "\\begin{tikzpicture}[node distance=1.5cm, auto, >=latex]"
  raw "\\node " <> selected 1 <> raw " (a) {1};"
  raw "\\node " <> selected 2 <> raw " (b) [right of=a] {2};"
  raw "\\node " <> selected 3 <> raw " (c) [right of=b] {3};"
  raw "\\node " <> selected 4 <> raw " (d) [right of=c] {4};"
  raw "\\node " <> selected 5 <> raw " (e) [right of=d] {5};"
  raw "\\node " <> selected 6 <> raw " (f) [right of=e] {6};"
  raw "\\node " <> selected 7 <> raw " (g) [right of=f] {7};"
  raw "\\end{tikzpicture}"
  raw "\\end{figure}"
  where
    selected s | s == sri  = "[che]"
    selected s             = "[int]"

section_risk_body :: Monad m => RiskSummary -> LaTeXT_ m
section_risk_body RiskSummary{..} = do
  "The summary risk indicator is a guide to the level of risk of this product compared to other products. It shows how likely it is that the product will lose money because of movements in the markets or because the Issuer and the Guarantor are not able to pay you.  We have classified this product as 5 out of 7, which is a medium-high risk class. This rates the potential losses from future performance at a medium-high level, and poor market conditions are very unlikely to impact the capacity of the Issuer and the Guarantor to pay you. To the extent the currency of the country in which you purchase this product or the account to which payments on this product are credited differs from the product currency, please be aware of the currency risk. You will receive payments in a different currency, so the final return you will get depend on the exchange rate between the two currencies. This risk is not considered in the indicator shown above. In the case of the physical delivery of the Underlying, price losses may arise even after the Valuation Date until the Underlying is credited to your securities account. This product does not include any protection from future market performance so you could lose some or all of your investment. If the Issuer and the Guarantor are not able to pay you what is owed, you could lose your entire investment."

stress_scenario_label :: Monad m => Language -> LaTeXT_ m
stress_scenario_label EN = "Stress scenario"
stress_scenario_label _  = "Stress scenario"

unfavourable_scenario_label :: Monad m => Language -> LaTeXT_ m
unfavourable_scenario_label EN = "Unfavourable scenario"
unfavourable_scenario_label _  = "Unfavourable scenario"

moderate_scenario_label :: Monad m => Language -> LaTeXT_ m
moderate_scenario_label EN = "Moderate scenario"
moderate_scenario_label _  = "Moderate scenario"

favourable_scenario_label :: Monad m => Language -> LaTeXT_ m
favourable_scenario_label EN = "Favourable scenario"
favourable_scenario_label _  = "Favourable scenario"

might_get_after_costs :: Monad m => Language -> LaTeXT_ m
might_get_after_costs EN = "What you might get back after costs"
might_get_after_costs _  = "What you might get back after costs"

avg_return_year :: Monad m => Language -> LaTeXT_ m
avg_return_year EN = "Average return each year"
avg_return_year _  = "Average return each year"

format :: (Show a, Monad m) => a -> LaTeXT_ m
format = fromString . show

(&%) :: Monad m => LaTeXT_ m -> Maybe (LaTeXT_ m) -> LaTeXT_ m
(&%) a (Just b) = a & b
(&%) a Nothing  = a

scenario_table :: Monad m => Language -> Scenario -> Maybe Scenario -> Maybe Scenario -> LaTeXT_ m
scenario_table l o h f = do
  tabular Nothing table_specs $ do
    hline
    stress_scenario_label l       & might_get_after_costs l &% strs_one_year &% strs_rhp_half & strs_rhp <> lnbk
    mempty                        & avg_return_year l       &% Nothing       &% Nothing       & "todo"   <> lnbk
    cline 2 n
    unfavourable_scenario_label l & might_get_after_costs l &% unf_one_year  &% unf_rhp_half  & unf_rhp  <> lnbk
    mempty                        & avg_return_year l       &% Nothing       &% Nothing       & "todo"   <> lnbk
    cline 2 n
    moderate_scenario_label l     & might_get_after_costs l &% mod_one_year  &% mod_rhp_half  & mod_rhp  <> lnbk
    mempty                        & avg_return_year l       &% Nothing       &% Nothing       & "todo"   <> lnbk
    cline 2 n
    favourable_scenario_label l   & might_get_after_costs l &% fav_one_year  &% fav_rhp_half  & fav_rhp  <> lnbk
    mempty                        & avg_return_year l       &% Nothing       &% Nothing       & "todo"   <> lnbk
    cline 2 n
  where
    n = 5
    table_specs
      | n == 5    = [Separator "", ParColumnTop "6.5cm", ParColumnTop "7cm", ParColumnTop "1.5cm", ParColumnTop "1.5cm", ParColumnTop "1.5cm"]
      | otherwise = [Separator "", ParColumnTop "7cm", ParColumnTop "8cm", ParColumnTop "1.5cm", ParColumnTop "1.5cm"]

    strs_rhp      = format $ stress o
    strs_rhp_half = fmap (format . stress) h
    strs_one_year = fmap (format . stress) f
    unf_rhp       = format $ unfavourable o
    unf_rhp_half  = fmap (format . unfavourable) h
    unf_one_year  = fmap (format . unfavourable) f
    mod_rhp       = format $ moderate o
    mod_rhp_half  = fmap (format . moderate) h
    mod_one_year  = fmap (format . moderate) f
    fav_rhp       = format $ favourable o
    fav_rhp_half  = fmap (format . favourable) h
    fav_one_year  = fmap (format . favourable) f

-- | Performance scenarios
section_performance_scenarios :: Monad m => Language -> Contract -> RiskSummary -> LaTeXT_ m
section_performance_scenarios l c r@RiskSummary{..} = do
  section' "Performance Scenarios"
  scenario_table l scenario_rhp scenario_rhp_half scenario_one_year
  -- newline
  -- newline
  -- section_performance_body EN r

section_performance_body :: Monad m => Language -> RiskSummary -> LaTeXT_ m
section_performance_body EN r = do
  "This table shows the money you could get back over the next 1 year and 3 months, under different scenarios, assuming that you invest USD 10'000. The scenarios shown illustrate how your investment could perform. You can compare them with the scenarios of other products. The scenarios presented are an estimate of future performance based on evidence from the past on how the value of this investment varies, and are not an exact indicator. What you get will vary depending on how the market performs and how long you keep the product. The stress scenario shows what you might get back in extreme market circumstances, and it does not take into account the situation where the Issuer and the Guarantor are not able to pay you. The figures shown include all the costs of the product itself, but may not include all the costs that you pay to your advisor or distributor. The figures do not take into account your personal tax situation, which may also affect how much you get back."


---------------------------------------------------------------------------------------------------
-- | What happens if the issuer is unable to pay out
---------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------
-- | What are the costs
---------------------------------------------------------------------------------------------------

table_row :: Monad m => LaTeXT_ m -> LaTeXT_ m -> Double -> LaTeXT_ m -> LaTeXT_ m -> Double -> LaTeXT_ m -> LaTeXT_ m
table_row l l1 n1 c1 l2 n2 c2 = do
      hline
      textbf l & textbf l1 & textbf (format n1) & c1 <> lnbk <> cline 2 4
      mempty   & textbf l2 & textbf (format n2) & c2 <> lnbk
  where
    format = fromString . show

section_costs :: Monad m => Language -> Contract -> LaTeXT_ m
section_costs DE Contract{..} = do
  section' "Was sind die Kosten?"
  tabular Nothing [Separator "", ParColumnTop "3cm", ParColumnTop "4.5cm", ParColumnTop "1.5cm", ParColumnTop "9cm"] body
  where
    body = do
      table_row
        "Einmalige Kosten"
        "Einstiegskosten" (entry costs)
        "Auswirkungen der Kosten, die Sie zahlen müssen, wenn Sie Ihre Anlage tätigen"
        "Austiegeskosten" (exit costs)
        "Auswirkungen der Kosten, die anfallen, wenn Sie bei Fälligkeit aus Ihrer Anlage aussteigen"
      table_row
        "Laufende Kosten"
        "Portfolio-Transkationskosten" (entry costs)
        "Auswirkungen der Kosten, die dafür anfallen, dass wir für das Produkt zugrunde liegende Anlagen kaufen und verkaufen"
        "Sonstige laufende Kosten" (exit costs)
        "Auswirkungen der Kosten, die ..."
      table_row
        "Zusätzliche Kosten"
        "Erfolgsgebühr" (entry costs)
        ""
        "Carried Interest" (exit costs)
        ""

section_costs EN Contract{..} = do
  section' "What are the costs?"
  tabular Nothing [Separator "", ParColumnTop "3cm", ParColumnTop "4.5cm", ParColumnTop "1.5cm", ParColumnTop "9cm"] body
  where
    body = do
      table_row
        "One-off costs"
        "Entry costs" (entry costs)
        "Auswirkungen der Kosten, die Sie zahlen müssen, wenn Sie Ihre Anlage tätigen"
        "Exit costs" (exit costs)
        "Auswirkungen der Kosten, die anfallen, wenn Sie bei Fälligkeit aus Ihrer Anlage aussteigen"
      table_row
        "Laufende Kosten"
        "Portfolio-Transkationskosten" (entry costs)
        "Auswirkungen der Kosten, die dafür anfallen, dass wir für das Produkt zugrunde liegende Anlagen kaufen und verkaufen"
        "Sonstige laufende Kosten" (exit costs)
        "Auswirkungen der Kosten, die ..."
      table_row
        "Zusätzliche Kosten"
        "Erfolgsgebühr" (entry costs)
        ""
        "Carried Interest" (exit costs)
        ""

---------------------------------------------------------------------------------------------------
-- | How long should I hold it and can I take money out early
---------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------
-- | How can I complain
---------------------------------------------------------------------------------------------------

section_how_can_i_complain :: Monad m => Language -> Contract -> LaTeXT_ m
section_how_can_i_complain DE _ = section' "Wie kann sich der Investor beschweren?" -- TODO
section_how_can_i_complain FR _ = section' "Quelle démarche l'investisseur doit-il suivre pour soumettre une réclamation?" -- TODO
section_how_can_i_complain IT _ = section' "Come può l’investitore presentare reclamo?" -- TODO
section_how_can_i_complain EN Contract{..} = do
  section' "How can the investor complain?"
  "Any complaint regarding the person advising on, or selling, the product can be submitted directly to that person via the relevant website. Any complaint regarding the product, this document or the conduct of the Manufacturer and/or the Issuer of this product can be submitted in text form (e.g. by letter or e-mail) to Bank Vontobel AG, Structured Products, Bleicherweg 21, 8022 Zurich, Switzerland, email: zertifikate.ch@vontobel.com, website: https://derinet.vontobel.com."

---------------------------------------------------------------------------------------------------
-- | Other relevant information
---------------------------------------------------------------------------------------------------

name' :: Monad m => Issuer -> LaTeXT_ m
name' = fromString . issuer_name

web' :: Monad m => Issuer -> LaTeXT_ m
web' = url . createURL . issuer_web

section_other_relevant_information :: Monad m => Language -> Contract -> LaTeXT_ m
section_other_relevant_information DE Contract{..} = do
  section' "Weitere relevante Informationen"
  "Dieses Basisinformationsblatt enthält nicht alle Informationen zu diesem Produkt. Die rechtlich verbindlichen Bedingungen des Produkts sowie eine detaillierte Beschreibung der mit diesem Produkt verbundenen Risiken und Chancen entnehmen Sie bitte dem zugrunde liegenden Prospekt. Der Prospekt ist auf " <> web' issuer <> " erhältlich und kann kostenlos in Papierform bei der " <> name' issuer <> " angefordert werden. Die in diesem Dokument enthaltenen Informationen stellen keine Empfehlung zum Kauf oder Verkauf des Produkts dar und sind kein Ersatz für eine individuelle Beratung durch die Bank oder den Berater des Anlegers. Jede aktualisierte Version dieses Basisinformationsblatts wird veröffentlicht unter: " <> web' issuer

section_other_relevant_information FR Contract{..} = do
  section' "Autres informations pertinentes"
  "Ce document d'information clé ne contient pas toutes les informations relatives à ce produit. Veuillez vous reporter au prospectus sous-jacent pour connaître les conditions juridiquement contraignantes du produit ainsi qu'une description détaillée des risques et des avantages associés à ce produit. Le prospectus est disponible sur " <> web' issuer <> ", et une copie papier de ce document peut être obtenue gratuitement auprès de la " <> name' issuer <> ". Les informations contenues dans ce document d'information clé ne constituent pas une recommandation d'achat ou de vente du produit et ne remplacent pas une consultation individuelle avec la banque ou le conseiller de l'investisseur. Toute version mise à jour de ce document d'information clé sera publiée sur le site: " <> web' issuer

section_other_relevant_information IT Contract{..} = do
  section' "Altre informazioni"
  "Questo documento contenente le informazioni chiave non contiene tutte le informazioni relative a questo prodotto. Per le condizioni generali giuridicamente vincolanti del prodotto e per una descrizione dettagliata dei rischi e dei vantaggi associati a questo prodotto, consultare il relativo prospetto informativo. Il prospetto è disponibile su " <> web' issuer <> " e una copia cartacea di questo documento può essere ottenuta gratuitamente presso " <> name' issuer <> ". Le informazioni contenute in questo documento non costituiscono una raccomandazione per l'acquisto o la vendita del prodotto e non sostituiscono la consultazione individuale con la banca o il consulente di investor Le versioni aggiornate di questo documento informativo saranno pubblicate su: " <> web' issuer

section_other_relevant_information EN Contract{..} = do
  section' "Other relevant information"
  "This key information document does not contain all information relating to this product. Please refer to the underlying prospectus for the legally binding terms and conditions of the product as well as a detailed description of the risks and rewards associated with this product. The prospectus is available on " <> web' issuer <> ", and a paper copy of this document may be obtained free of charge from " <> name' issuer <> ". The information contained in this key information document does not constitute a recommendation to buy or sell the product and is no substitute for individual consultation with the investor's bank or advisor. Any updated version of this key information document will be published on: " <> web' issuer
