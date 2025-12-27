package com.gdn.x.productcategorybase.solr.model;

import java.util.Date;

/**
 * Created by Kesha on 26/04/16.
 */
public class  DeltaProduct {
  private String id;
  private String name;
  private String productCode;
  private ActionType actionType;
  private Date updatedDate;

  public DeltaProduct(String id, String name, String productCode, ActionType actionType,
      Date updatedDate) {
    this.id = id;
    this.name = name;
    this.productCode = productCode;
    this.actionType = actionType;
    this.updatedDate = updatedDate;
  }

  public String getId() {
    return id;
  }

  public String getName() {
    return name;
  }

  public String getProductCode() {
    return productCode;
  }

    public ActionType getActionType() {
      return actionType;
    }

  public Date getUpdatedDate() {
    return updatedDate;
  }

  @Override
  public boolean equals(Object objToCompare) {
    if (this == objToCompare) {
      return true;
    }
    if (objToCompare == null || getClass() != objToCompare.getClass()) {
      return false;
    }
    DeltaProduct that = (DeltaProduct) objToCompare;
    if (id != null ? !id.equals(that.id) : that.id != null) {
      return false;
    }
    return actionType == that.actionType;

  }

  @Override
  public int hashCode() {
    int result = id != null ? id.hashCode() : 0;
    result = 31 * result + (actionType != null ? actionType.hashCode() : 0);
    return result;
  }
}
