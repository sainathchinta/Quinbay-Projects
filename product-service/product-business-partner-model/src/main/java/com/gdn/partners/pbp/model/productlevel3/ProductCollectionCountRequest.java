package com.gdn.partners.pbp.model.productlevel3;

import java.util.Date;

public class ProductCollectionCountRequest {
  
  private String storeId;
  private String categoryCode;
  private String businessPartnerCode;
  private String keyword;
  private Date dateStart;
  private Date dateEnd;
  private boolean activated;
  private boolean viewable;
  private String filterDateType;
  
  public ProductCollectionCountRequest() {
    super();
  }
  public String getStoreId() {
    return storeId;
  }
  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }
  public String getCategoryCode() {
    return categoryCode;
  }
  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }
  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }
  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }
  public String getKeyword() {
    return keyword;
  }
  public void setKeyword(String keyword) {
    this.keyword = keyword;
  }
  public Date getDateStart() {
    return dateStart;
  }
  public void setDateStart(Date dateStart) {
    this.dateStart = dateStart;
  }
  public Date getDateEnd() {
    return dateEnd;
  }
  public void setDateEnd(Date dateEnd) {
    this.dateEnd = dateEnd;
  }
  public boolean isActivated() {
    return activated;
  }
  public void setActivated(boolean activated) {
    this.activated = activated;
  }
  public boolean isViewable() {
    return viewable;
  }
  public void setViewable(boolean viewable) {
    this.viewable = viewable;
  }
  public String getFilterDateType() {
    return filterDateType;
  }
  public void setFilterDateType(String filterDateType) {
    this.filterDateType = filterDateType;
  }
  
}
