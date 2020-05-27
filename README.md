# Pf-project
Implementarea unui asistent pentru demonstratii de identitati in algebre booleene.
Exemplu de rulare:
> boolean_prove "xy + xyz = xy" // apelez programul
Goal: xy + xyz = xyz
> distributivity and or // intai aplic legea distributivitatii
// lui and fata de or
Goal: x(y + yz) = xyz
> distributivity and or // inca o data legea distributivitatii
Goal: xy(1 + z) = xyz
> annihilator or // aplic legea absorbtiei pentru or
Goal: xyz = xyz
> reflexivity // done
Done.
