package com.gdn.x.product.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Map;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class VideoCompressionEventModel extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = -7950207754965330982L;
  private String videoId;
  private String oldVideoId;
  private String clientId;
  private String ownerId;
  private boolean markForDelete;
  private Map<String, String> additionalFields;
}
