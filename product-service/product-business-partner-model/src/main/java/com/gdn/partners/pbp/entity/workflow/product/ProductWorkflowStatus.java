package com.gdn.partners.pbp.entity.workflow.product;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ProductWorkflowStatus implements Serializable {

  private static final long serialVersionUID = -6150056594703332514L;
  private String productCode;
  private List<String> states = new ArrayList<String>();
  private Map<String, Boolean> status = new HashMap<String, Boolean>();
  private boolean reviewPending;

  public ProductWorkflowStatus() {}

  public ProductWorkflowStatus(String productCode, List<String> states, Map<String, Boolean> status) {
    super();
    this.productCode = productCode;
    this.states = states;
    this.status = status;
  }

  public ProductWorkflowStatus(String productCode, List<String> states, Map<String, Boolean> status,
      boolean reviewPending) {
    this(productCode, states, status);
    this.reviewPending = reviewPending;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public List<String> getStates() {
    return states;
  }

  public void setStates(List<String> states) {
    this.states = states;
  }

  public Map<String, Boolean> getStatus() {
    return status;
  }

  public void setStatus(Map<String, Boolean> status) {
    this.status = status;
  }

  public boolean isReviewPending() {
    return reviewPending;
  }

  public void setReviewPending(boolean reviewPending) {
    this.reviewPending = reviewPending;
  }

  @Override
  public String toString() {
    return String.format("ProductWorkflowStatus [productCode=%s, states=%s, status=%s]", productCode, states, status);
  }

}
