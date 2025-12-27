package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.List;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

/**
 * Created by Vishal on 05/10/17.
 */
public class PristineDataItemModel extends GdnBaseDomainEventModel implements Serializable{

  private static final long serialVersionUID = 2272377122184086694L;

  private List<String> itemCodes;

  public List<String> getItemCodes() {
    return itemCodes;
  }

  public void setItemCodes(List<String> itemCodes) {
    this.itemCodes = itemCodes;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("itemCodes", itemCodes).toString();
  }
}
