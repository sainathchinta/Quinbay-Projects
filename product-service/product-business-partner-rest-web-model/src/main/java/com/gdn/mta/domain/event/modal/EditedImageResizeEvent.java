package com.gdn.mta.domain.event.modal;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.micro.graphics.web.model.ImageRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class EditedImageResizeEvent implements Serializable {

  private static final long serialVersionUID = 5124338404418843547L;
  private String productCode;
  private String storeId;
  private List<ImageRequest> imageRequests;
}
