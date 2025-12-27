package com.gdn.x.product.model.vo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleMasterDataDetailWithProductAndItemsV2ResponseVo {

  private Map<String, SimpleMasterDataProductVO> masterDataProducts =
      new HashMap<String, SimpleMasterDataProductVO>();
  private Map<String, SimpleMasterDataItemVO> masterDataItems =
      new HashMap<String, SimpleMasterDataItemVO>();
  private List<SimpleProductAndItemsAndItemPickupPointV0>
      productAndItems = new ArrayList<SimpleProductAndItemsAndItemPickupPointV0>();
  private long totalL5Count;
}
