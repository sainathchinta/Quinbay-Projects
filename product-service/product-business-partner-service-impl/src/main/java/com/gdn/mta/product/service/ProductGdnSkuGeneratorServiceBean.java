package com.gdn.mta.product.service;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.service.validator.ProductGdnSkuGeneratorValidator;

@Component
public class ProductGdnSkuGeneratorServiceBean implements ProductGdnSkuGeneratorService {

  @Autowired
  private ProductGdnSkuGeneratorValidator validator;

  @Autowired
  private ProductGdnSkuGenerator productGdnSkuGenerator;

  @Override
  public void generateGdnSkuOnProduct(ProductBusinessPartner productBusinessPartner, boolean isMppFlow) {
    validator.validateGenerateProductGdnSkuData(productBusinessPartner);
    String productGdnSku =
        productGdnSkuGenerator.generateProductGdnSku(productBusinessPartner.getBusinessPartnerId());
    productBusinessPartner.setGdnProductSku(productGdnSku);
      int itemNo = 1;
    if (isMppFlow) {
      Map<String, String> tempItemSkuAndNewItemSkuMap = new HashMap<>();
      for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner
          .getProductItemBusinessPartners()) {
        String tempItemSku = productItemBusinessPartner.getGdnProductItemSku();
        if (!tempItemSkuAndNewItemSkuMap.containsKey(productItemBusinessPartner.getGdnProductItemSku())) {
          validator.validateGenerateProductItemGdnSkuData(itemNo);
          String generateProductItemGdnSku = productGdnSkuGenerator.generateProductItemGdnSku(productGdnSku, itemNo);
          productItemBusinessPartner.setGdnProductItemSku(generateProductItemGdnSku);
          if(StringUtils.isNotBlank(tempItemSku)) {
            tempItemSkuAndNewItemSkuMap.putIfAbsent(tempItemSku, generateProductItemGdnSku);
          }
          itemNo++;
        } else {
          productItemBusinessPartner
              .setGdnProductItemSku(tempItemSkuAndNewItemSkuMap.get(productItemBusinessPartner.getGdnProductItemSku()));
        }
      }
    } else {
    for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner
        .getProductItemBusinessPartners()) {
      validator.validateGenerateProductItemGdnSkuData(itemNo);
      productItemBusinessPartner.setGdnProductItemSku(productGdnSkuGenerator
          .generateProductItemGdnSku(productGdnSku, itemNo));
      itemNo++;
    }
  }}

  @Override
  public String convertToGeneratedGdnSkus(ProductBusinessPartner productBusinessPartner) {
    StringBuilder strBuilder = new StringBuilder();
    if (productBusinessPartner != null) {
      strBuilder.append("{" + productBusinessPartner.getProductName() + " : "
          + productBusinessPartner.getGdnProductSku() + ", productItems=");
      if (CollectionUtils.isNotEmpty(productBusinessPartner.getProductItemBusinessPartners())) {
        int currentIndex = 0;
        int lastIndex = productBusinessPartner.getProductItemBusinessPartners().size() - 1;
        for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner
            .getProductItemBusinessPartners()) {
          strBuilder.append("[" + productItemBusinessPartner.getProductItemId() + " : "
              + productItemBusinessPartner.getGdnProductItemSku() + "]");
          if (currentIndex < lastIndex) {
            strBuilder.append(", ");
            currentIndex++;
          }
        }
      }
      strBuilder.append("}");
    }
    return strBuilder.toString();
  }
}
