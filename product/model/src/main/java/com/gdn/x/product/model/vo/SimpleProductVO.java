package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.gdn.x.product.enums.CurationStatus;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductSpecialAttribute;

/**
 * Created by govind on 01/08/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(value = JsonInclude.Include.NON_EMPTY)
public class SimpleProductVO implements Serializable{

  private static final long serialVersionUID = 213280959747878604L;
  private String productCode;
  private String productSku;
  private int productTypeCode;
  private String merchantCode;
  private Set<String> productChannel = new HashSet<>();
  private boolean isSynchronized;
  private boolean archived;
  private boolean suspended;
  private boolean cncActive;
  private boolean markForDelete;
  private boolean off2OnActive;
  private List<ProductSpecialAttribute> productSpecialAttributes;
  private Double productScoreTotal;
  private SimpleAsyncMasterDataProductVO simpleAsyncMasterDataProduct;
  private PreOrderVO preOrder;
  private Map<String, String> categoryCodeAndCategoryNameMap;
  private boolean halalProduct;
  private String sizeChartCode;

  public SimpleProductVO() {}

  public SimpleProductVO(String productCode, String productSku, int productTypeCode, String merchantCode,
      Set<String> productChannel, boolean isSynchronized, List<ProductSpecialAttribute> productSpecialAttributes,
      Double productScoreTotal, SimpleAsyncMasterDataProductVO simpleAsyncMasterDataProduct, PreOrderVO preOrder) {
    this.productCode = productCode;
    this.productSku = productSku;
    this.productTypeCode = productTypeCode;
    this.merchantCode = merchantCode;
    this.productChannel = productChannel;
    this.isSynchronized = isSynchronized;
    this.productSpecialAttributes = productSpecialAttributes;
    this.productScoreTotal = productScoreTotal;
    this.simpleAsyncMasterDataProduct = simpleAsyncMasterDataProduct;
    this.preOrder = preOrder;
  }

  public SimpleProductVO(String productSku) {
    this.productSku = productSku;
  }

  public SimpleProductVO(String productSku, String productCode) {
    this.productSku = productSku;
    this.productCode = productCode;
  }

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

  public boolean isArchived() {
    return archived;
  }

  public void setArchived(boolean isArchived) {
    archived = isArchived;
  }

  public boolean isCncActive() {
    return cncActive;
  }

  public void setCncActive(boolean isCncActive) {
    cncActive = isCncActive;
  }

  public boolean isSuspended() {
    return suspended;
  }

  public void setSuspended(boolean isSuspended) {
    suspended = isSuspended;
  }

  public boolean isOff2OnActive() {
    return off2OnActive;
  }

  public void setOff2OnActive(boolean isOff2OnActive) {
    off2OnActive = isOff2OnActive;
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setMarkForDelete(boolean isMarkForDelete) {
    markForDelete = isMarkForDelete;
  }

  public SimpleAsyncMasterDataProductVO getSimpleAsyncMasterDataProduct() {
    return simpleAsyncMasterDataProduct;
  }

  public void setSimpleAsyncMasterDataProduct(
      SimpleAsyncMasterDataProductVO simpleAsyncMasterDataProduct) {
    this.simpleAsyncMasterDataProduct = simpleAsyncMasterDataProduct;
  }

  public List<ProductSpecialAttribute> getProductSpecialAttributes() {
    return productSpecialAttributes;
  }

  public void setProductSpecialAttributes(List<ProductSpecialAttribute> productSpecialAttributes) {
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

  public PreOrderVO getPreOrder() {
    return preOrder;
  }

  public void setPreOrder(PreOrderVO preOrder) {
    this.preOrder = preOrder;
  }

  public Map<String, String> getCategoryCodeAndCategoryNameMap() {
    return categoryCodeAndCategoryNameMap;
  }

  public void setCategoryCodeAndCategoryNameMap(Map<String, String> categoryCodeAndCategoryNameMap) {
    this.categoryCodeAndCategoryNameMap = categoryCodeAndCategoryNameMap;
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

  @JsonIgnore
  public static SimpleProductVO toSimpleProductVo(SimpleMasterDataProductVO simpleMasterDataProductVO, Product product){
    SimpleProductVO simpleProductVo = new SimpleProductVO();
    Set<String> productChannel = new HashSet<>();
    simpleProductVo.setProductCode(product.getProductCode());
    simpleProductVo.setMerchantCode(product.getMerchantCode());
    simpleProductVo.setProductSku(product.getProductSku());
    simpleProductVo.setProductSpecialAttributes(product.getProductSpecialAttributes());
    if(Objects.nonNull(product.getProductType())) {
      simpleProductVo.setProductTypeCode(product.getProductType().getCode());
    }
    simpleProductVo.setSynchronized(product.isSynchronized());
    if(!product.isSynchronized()) {
      SimpleAsyncMasterDataProductVO simpleAsyncMasterDataProductVO =
          SimpleAsyncMasterDataProductVO.toSimpleAsyncMasterDataProductVO(product);
      if (Objects.nonNull(simpleMasterDataProductVO) && CollectionUtils
          .isNotEmpty(simpleMasterDataProductVO.getMasterDataProductImages())) {
        simpleAsyncMasterDataProductVO
            .setMasterDataProductImages(simpleMasterDataProductVO.getMasterDataProductImages());
      }
      simpleProductVo.setSimpleAsyncMasterDataProduct(simpleAsyncMasterDataProductVO);
    }
    if (Objects.nonNull(product.getProductScore())) {
      simpleProductVo.setProductScoreTotal(product.getProductScore().getTotalScore());
    }
    if (product.isB2bActivated()) {
      productChannel.add(Constants.B2B);
    }
    if (product.getB2cActivated()) {
      productChannel.add(Constants.RETAIL);
    }
    simpleProductVo.setProductChannel(productChannel);
    simpleProductVo.setArchived(product.isArchived());
    simpleProductVo.setSuspended(product.isSuspended());
    simpleProductVo.setCncActive(product.isCncActivated());
    simpleProductVo.setMarkForDelete(product.isMarkForDelete());
    simpleProductVo.setOff2OnActive(product.isOff2OnChannelActive());
    simpleProductVo.setHalalProduct(CurationStatus.APPROVED.equals(product.getCurationStatus()));
    simpleProductVo.setSizeChartCode(product.getSizeChartCode());
    return simpleProductVo;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productCode", productCode).append("productSku", productSku)
        .append("productTypeCode", productTypeCode).append("merchantCode", merchantCode)
        .append("productChannel", productChannel).append("isSynchronized", isSynchronized)
        .append("simpleAsyncMasterDataProduct", simpleAsyncMasterDataProduct)
        .append("categoryCodeAndCategoryNameMap", categoryCodeAndCategoryNameMap)
        .append("sizeChartCode", sizeChartCode)
        .append("halalProduct", halalProduct).toString();
  }
}
