package com.gdn.x.product.domain.event.model;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class ItemImageChangeModel extends GdnBaseDomainEventModel {
  private String itemSKu;
  private List<MasterDataItemImage> itemImages = new ArrayList<>();
}
