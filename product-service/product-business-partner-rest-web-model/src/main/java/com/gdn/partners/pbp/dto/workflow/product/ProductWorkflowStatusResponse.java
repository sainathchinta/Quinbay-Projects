package com.gdn.partners.pbp.dto.workflow.product;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductWorkflowStatusResponse extends BaseResponse {

  private static final long serialVersionUID = -7199955058147291307L;
  private String productCode;
  private List<String> states = new ArrayList<String>();
  private Map<String, Boolean> status = new HashMap<String, Boolean>();
  private boolean reviewPending;

  public ProductWorkflowStatusResponse() {}

  public ProductWorkflowStatusResponse(String productCode, List<String> states, Map<String, Boolean> status) {
    super();
    this.productCode = productCode;
    this.states = states;
    this.status = status;
  }

  public ProductWorkflowStatusResponse(String productCode, List<String> states, Map<String, Boolean> status,
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
    return String.format("ProductWorkflowStatusResponse [productCode=%s, states=%s, status=%s]", productCode, states,
        status);
  }

}
