package com.gdn.micro.graphics.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ScaleImagesResponse extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -6108346759411106623L;
  
  private List<ImagePathResult> imagePathResult = new ArrayList<>();
  private String clientId;
  private String storeId;
  private String requestId;
  private String uniqueIdentifier;
}
