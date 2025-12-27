package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.rest.web.model.dto.SimpleMasterDataItemDTO;
import com.gdn.x.product.rest.web.model.dto.SimpleMasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.SimpleProductAndItemAndItemPickupPointDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleProductAndItemsMasterDataDetailV2Response extends BaseResponse {

  private static final long serialVersionUID = -9072435074485520452L;
  private Map<String, SimpleMasterDataProductDTO> masterDataProducts =
      new HashMap<String, SimpleMasterDataProductDTO>();
  private Map<String, SimpleMasterDataItemDTO> masterDataItems =
      new HashMap<String, SimpleMasterDataItemDTO>();
  private List<SimpleProductAndItemAndItemPickupPointDTO> productAndItems = new ArrayList<SimpleProductAndItemAndItemPickupPointDTO>();

  private PageMetaData pageMetaData = new PageMetaData();
}
