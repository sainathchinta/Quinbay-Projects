package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.model.vo.PreOrderVO;
import com.gdn.x.product.model.vo.ProductItemsItemPickupPoints;
import com.gdn.x.product.model.vo.ProductScoreVo;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.enums.EditChangeType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class EditProductDetailDTO extends ProductItemsItemPickupPoints {

  private static final long serialVersionUID = 7484042457003534998L;

  private boolean updateCategory;
  private PreOrderVO preOrderVO;
  private boolean freeSampleToggledOn;
  private boolean off2OnActiveChanged;
  private boolean productScoreChanged;
  private ProductScoreVo existingProductScore;
  private boolean productUpdated;
  private List<ItemChangeEventType> itemChangeEventTypeList = new ArrayList<>();
  private EditChangeType editChangeType;
  private List<AuditTrailDto> auditTrailDtoListForBundling = new ArrayList<>();
  private long cncItemCount;
}
