package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

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
public class SimpleProductAndItemAndItemPickupPointDTO implements Serializable {

  private static final long serialVersionUID = -6174844862027837370L;
  private SimpleProductResponseDTO simpleProduct;
  private List<SimpleItemV2ResponseDTO> simpleItems = new ArrayList<SimpleItemV2ResponseDTO>();
  private List<SimpleItemPickupPointResponseDTO> itemPickupPoints = new ArrayList<SimpleItemPickupPointResponseDTO>();
}
