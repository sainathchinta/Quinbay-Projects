package com.gdn.x.product.rest.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@AllArgsConstructor
@NoArgsConstructor
@Data
public class BasicProductAndItemEditedDetailsDTO {

  private List<ItemPickupPoint> itemPickupPoints;
  private boolean isFbbFlagChangedAtL3Level;
}
