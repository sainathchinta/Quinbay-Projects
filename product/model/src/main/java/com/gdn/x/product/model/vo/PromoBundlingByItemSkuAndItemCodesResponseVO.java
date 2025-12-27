package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;

/**
 * Created by w.william on 3/15/2018.
 */
public class PromoBundlingByItemSkuAndItemCodesResponseVO {

    private PromoBundlingDetailResponseVO promoBundling;
    private int totalComboRule;
    private int totalWholesaleRule;

    public PromoBundlingByItemSkuAndItemCodesResponseVO() {
    }

    public boolean equals(Object obj) {
      return GdnObjects.equals(this, obj);
    }

    public int getTotalComboRule() {
      return this.totalComboRule;
    }

    public PromoBundlingDetailResponseVO getPromoBundling() {
      return this.promoBundling;
    }

    public int getTotalWholesaleRule() {
      return this.totalWholesaleRule;
    }

    public int hashCode() {
      return GdnObjects.hashCode(this);
    }

    public void setTotalComboRule(int totalComboRule) {
      this.totalComboRule = totalComboRule;
    }

    public void setPromoBundling(PromoBundlingDetailResponseVO promoBundling) {
      this.promoBundling = promoBundling;
    }

    public void setTotalWholesaleRule(int totalWholesaleRule) {
      this.totalWholesaleRule = totalWholesaleRule;
    }

    public String toString() {
      StringBuilder sb = new StringBuilder("PromoBundlingByItemSkuAndItemCodesResponse{");
      sb.append("promoBundling=").append(this.promoBundling);
      sb.append(", totalComboRule=").append(this.totalComboRule);
      sb.append(", totalWholesaleRule=").append(this.totalWholesaleRule);
      sb.append('}');
      return sb.toString();
    }
    }
