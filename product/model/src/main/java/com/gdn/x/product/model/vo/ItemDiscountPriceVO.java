package com.gdn.x.product.model.vo;

import java.io.Serializable;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.gdn.x.product.model.entity.DiscountPrice;

/**
 * Created by govind on 02/05/2019 AD.
 */
public class ItemDiscountPriceVO implements Serializable{

  private static final long serialVersionUID = -2505762671482562572L;

  private DiscountPrice discountPrice;
  private boolean merchantPromoDiscount;

  public DiscountPrice getDiscountPrice() {
    return discountPrice;
  }

  public void setDiscountPrice(DiscountPrice discountPrice) {
    this.discountPrice = discountPrice;
  }

  public boolean isMerchantPromoDiscount() {
    return merchantPromoDiscount;
  }

  public void setMerchantPromoDiscount(boolean merchantPromoDiscount) {
    this.merchantPromoDiscount = merchantPromoDiscount;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("discountPrice", discountPrice)
        .append("merchantPromoDiscount", merchantPromoDiscount).toString();
  }
}
