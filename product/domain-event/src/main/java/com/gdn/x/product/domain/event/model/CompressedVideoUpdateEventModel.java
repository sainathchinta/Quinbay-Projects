package com.gdn.x.product.domain.event.model;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.Map;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class CompressedVideoUpdateEventModel extends GdnBaseDomainEventModel
  implements Serializable {
  @Serial
  private static final long serialVersionUID = -6809096261137662542L;
  private String videoId;
  private String finalUrl;
  private String coverImagePath;
  private String sourceUrl;
  private String videoName;
  private String clientId;
  private Map<String, String> additionalFields;
}