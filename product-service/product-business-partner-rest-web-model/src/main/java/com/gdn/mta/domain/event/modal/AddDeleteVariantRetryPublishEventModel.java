package com.gdn.mta.domain.event.modal;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AddDeleteVariantRetryPublishEventModel extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = 3160830807911439006L;
  private String productCode;
}
