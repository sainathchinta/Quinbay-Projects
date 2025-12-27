package com.gdn.x.product.rest.web.model;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemV2;
import com.gdn.x.product.rest.web.model.enums.ApiErrorCode;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class CombinedEditItemResponse extends BaseResponse {

  private static final long serialVersionUID = 153820378401008006L;

  private ApiErrorCode apiErrorCode;
  private Boolean isArchived;
  private boolean cncActivated;
  private List<ItemPickupPointVo> updatedItemPickupPointsVo = new ArrayList<>();
  private List<ItemV2> updatedItems = new ArrayList<>();
  private Long l3Version;
  private List<ItemPickupPointVo> allUpdatedItemPickupPointsVo = new ArrayList<>();

  public CombinedEditItemResponse(ApiErrorCode apiErrorCode, Boolean isArchived, boolean cncActivated,
      List<ItemPickupPointVo> updatedItemPickupPointsVo, List<ItemV2> updatedItems) {
    this.apiErrorCode = apiErrorCode;
    this.isArchived = isArchived;
    this.cncActivated = cncActivated;
    this.updatedItemPickupPointsVo = updatedItemPickupPointsVo;
    this.updatedItems = updatedItems;
  }

  public CombinedEditItemResponse(ApiErrorCode apiErrorCode, Boolean isArchived, boolean cncActivated,
      List<ItemPickupPointVo> updatedItemPickupPointsVo, List<ItemV2> updatedItems, Long l3Version) {
    this.apiErrorCode = apiErrorCode;
    this.isArchived = isArchived;
    this.cncActivated = cncActivated;
    this.updatedItemPickupPointsVo = updatedItemPickupPointsVo;
    this.updatedItems = updatedItems;
    this.l3Version = l3Version;
  }
}
