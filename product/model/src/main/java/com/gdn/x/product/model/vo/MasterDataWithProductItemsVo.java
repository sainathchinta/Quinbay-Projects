package com.gdn.x.product.model.vo;

import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Data
@NoArgsConstructor
public class MasterDataWithProductItemsVo {

  private Map<String, MasterDataProduct> masterDataProducts = new HashMap<>();
  private Map<String, MasterDataItem> masterDataItems = new HashMap<>();
  private List<ProductItemsVo> productItemsVos = new ArrayList<>();
  private long activeProductCount;
}
