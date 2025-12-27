package com.gdn.x.mta.distributiontask.domain.event.model;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class PDTProductSolrDeleteDomainEventModel extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 1526813343203952389L;
  private List<String> productCodes;
}
