package com.gdn.mta.product.service.validator;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductBusinessPartner;

@Component
public class ProductGdnSkuGeneratorValidatorBean implements ProductGdnSkuGeneratorValidator {

  @Override
  public void validateGenerateProductGdnSkuData(ProductBusinessPartner productBusinessPartner) {
   if (!isValidGenerateProductGdnSkuData(productBusinessPartner))
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          "Invalid data for generate product GDNSku");

  }

  @Override
  public boolean isValidGenerateProductGdnSkuData(ProductBusinessPartner productBusinessPartner) {
    boolean isValid = true;
    if (productBusinessPartner == null)
      isValid = false;
    else if (productBusinessPartner.getBusinessPartnerId() == null
        || StringUtils.isBlank(productBusinessPartner.getBusinessPartnerId()))
      isValid = false;
    
    return isValid;
  }

  @Override
  public boolean isValidGenerateProductItemGdnSkuData(int itemNo) {
    return itemNo > 0;
  }

  @Override
  public void validateGenerateProductItemGdnSkuData(int itemNo) {
    if (!isValidGenerateProductItemGdnSkuData(itemNo))
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, "Invalid product item number");
  }

}
