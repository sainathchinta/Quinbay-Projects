package com.gdn.mta.product.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.service.util.ProductGdnSkuGeneratorUtil;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.outbound.xbp.feign.XbpFeign;
@Component
public class ProductGdnSkuGeneratorBean implements ProductGdnSkuGenerator {

  @Autowired
  private ApplicationProperties applicationProperties;

  @Autowired
  private ProductGdnSkuGeneratorUtil generatorUtil;

  @Autowired
  private XbpFeign xbpFeign;

  private static final Logger log = LoggerFactory.getLogger(ProductGdnSkuGeneratorBean.class);

  @Override
  public String generateProductGdnSku(String businessPartnerCode) {
    try {
      Long productCounter = xbpFeign.incrementAndGetProductCounter(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), businessPartnerCode).getValue().getCounter();
      return generatorUtil.appendWithSerial(businessPartnerCode.toUpperCase(), productCounter,
          Integer.valueOf(applicationProperties.getSkuDigits()));
    } catch (Exception e) {
      log.error(
          "Error when invoking generateProductGdnSku at ProductGdnSkuGeneratorImpl\n Business partner code:"
              + businessPartnerCode, e);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "Error generate product gdn sku");
    }
  }

  @Override
  public String generateProductItemGdnSku(String productSku, int itemNo) {
    return generatorUtil.appendWithSerial(productSku, itemNo == 0 ? itemNo + 1 : itemNo,
        Integer.valueOf(applicationProperties.getSkuDigits()));
  }

}
