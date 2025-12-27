package com.gdn.partners.pdt.model.configuration.distribution;

public enum PriorityType {

  INITIATOR(1),
  BUSINESS_PARTNER_CODE(2),
  CATEGORY_CODE(3);

  private int priority;

  private PriorityType(int priority) {
    this.priority = priority;
  }

  public int getPriority() {
    return priority;
  }

}
