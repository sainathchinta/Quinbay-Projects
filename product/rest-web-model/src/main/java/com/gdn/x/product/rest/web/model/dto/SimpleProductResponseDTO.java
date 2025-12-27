package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

/**
 * Created by govind on 16/08/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(value = JsonInclude.Include.NON_EMPTY)
public class SimpleProductResponseDTO implements Serializable{

  private static final long serialVersionUID = -4938904100708882433L;
  private String productCode;
  private String productSku;
  private int productTypeCode;
  private String merchantCode;
  private Set<String> productChannel = new HashSet<>();
  private boolean isSynchronized;
  private List<ProductSpecialAttributeDTO> productSpecialAttributes;
  private SimpleAsyncMasterDataProductDTO simpleAsyncMasterDataProduct;
  private Double productScoreTotal;
  private PreOrderDTO preOrder;
  private boolean halalProduct;
  private String sizeChartCode;

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getProductSku() {
    return productSku;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public int getProductTypeCode() {
    return productTypeCode;
  }

  public void setProductTypeCode(int productTypeCode) {
    this.productTypeCode = productTypeCode;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public boolean isSynchronized() {
    return isSynchronized;
  }

  public void setSynchronized(boolean aSynchronized) {
    isSynchronized = aSynchronized;
  }

  public SimpleAsyncMasterDataProductDTO getSimpleAsyncMasterDataProduct() {
    return simpleAsyncMasterDataProduct;
  }

  public void setSimpleAsyncMasterDataProduct(
      SimpleAsyncMasterDataProductDTO simpleAsyncMasterDataProduct) {
    this.simpleAsyncMasterDataProduct = simpleAsyncMasterDataProduct;
  }

  public List<ProductSpecialAttributeDTO> getProductSpecialAttributes() {
    return productSpecialAttributes;
  }

  public void setProductSpecialAttributes(List<ProductSpecialAttributeDTO> productSpecialAttributes) {
    this.productSpecialAttributes = productSpecialAttributes;
  }

  public Set<String> getProductChannel() {
    return productChannel;
  }

  public void setProductChannel(Set<String> productChannel) {
    this.productChannel = productChannel;
  }

  public Double getProductScoreTotal() {
    return productScoreTotal;
  }

  public void setProductScoreTotal(Double productScoreTotal) {
    this.productScoreTotal = productScoreTotal;
  }

  public PreOrderDTO getPreOrder() {
    return preOrder;
  }

  public void setPreOrder(PreOrderDTO preOrder) {
    this.preOrder = preOrder;
  }

  public boolean isHalalProduct() {
    return halalProduct;
  }

  public void setHalalProduct(boolean halalProduct) {
    this.halalProduct = halalProduct;
  }

  public String getSizeChartCode() {
    return sizeChartCode;
  }

  public void setSizeChartCode(String sizeChartCode) {
    this.sizeChartCode = sizeChartCode;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productCode", productCode).append("productSku", productSku)
        .append("productTypeCode", productTypeCode).append("merchantCode", merchantCode)
        .append("isSynchronized", isSynchronized).append("productSpecialAttributes", productSpecialAttributes)
        .append("simpleAsyncMasterDataProduct", simpleAsyncMasterDataProduct)
        .append("productChannel", productChannel)
        .append("productScoreTotal", productScoreTotal)
        .append("sizeChartCode", sizeChartCode)
        .append("preOrder", preOrder).append("halalProduct",halalProduct).toString();
  }
}
