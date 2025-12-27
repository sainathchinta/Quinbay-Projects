package com.gdn.x.product.service.impl;

import com.gdn.x.product.service.api.SkuValidator;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
public class SkuValidatorImpl implements SkuValidator {

  private static final Pattern ITEM_SKU_PATTERN =
      Pattern.compile(SkuValidator.ITEM_SKU_PATTERN);
  private static final Pattern PRODUCT_SKU_PATTERN =
      Pattern.compile(SkuValidator.PRODUCT_SKU_PATTERN);
  private static final Pattern PRISTINE_SHORT_ID_PATTERN =
      Pattern.compile(SkuValidator.PRISTINE_SHORT_ID_PATTERN);
  private static final Pattern PRODUCT_CODE_PATTERN =
      Pattern.compile(SkuValidator.PRODUCT_CODE_PATTERN);
  private static final Pattern ITEM_SKU_L5_PATTERN =
      Pattern.compile(SkuValidator.ITEM_SKU_L5_PATTERN);

  @Override
  public boolean isItemSku(String sku) {
    if(StringUtils.isNotBlank(sku)) {
      Matcher matcher = ITEM_SKU_PATTERN.matcher(sku);
      return matcher.matches();
    }
    return false;
  }

  @Override
  public boolean isProductSku(String sku) {
    Matcher matcher = PRODUCT_SKU_PATTERN.matcher(sku);
    return matcher.matches();
  }

  @Override
  public boolean isPristineId(String pristineId) {
    Matcher matcher = PRISTINE_SHORT_ID_PATTERN.matcher(pristineId);
    return matcher.matches();
  }

  @Override
  public boolean isProductCode(String productCode) {
    Matcher matcher = PRODUCT_CODE_PATTERN.matcher(productCode);
    return matcher.matches();
  }

  @Override
  public boolean isItemSkuL4OrL5(String sku) {
    if(StringUtils.isNotBlank(sku)) {
      Matcher itemSkuL4Matcher = ITEM_SKU_PATTERN.matcher(sku);
      Matcher itemSkuL5Matcher = ITEM_SKU_L5_PATTERN.matcher(sku);
      return itemSkuL4Matcher.matches() || itemSkuL5Matcher.matches();
    }
    return false;
  }
}
