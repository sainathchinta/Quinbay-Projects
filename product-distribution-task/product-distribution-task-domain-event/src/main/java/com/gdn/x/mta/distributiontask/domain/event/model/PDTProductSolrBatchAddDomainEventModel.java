package com.gdn.x.mta.distributiontask.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PDTProductSolrBatchAddDomainEventModel extends GdnBaseDomainEventModel {

  private List<PDTProductSolrAddDomainEventModel> pdtProductSolrAddDomainEventModelList;
}
