package com.gdn.x.productcategorybase.dto.response;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemImageResponse extends BaseDTOResponse implements Serializable {

  private static final long serialVersionUID = 6652905286417794373L;

  private String itemCode;
  private String upcCode;
  private String itemName;
  private List<ImageResponse> imageResponses;
}
