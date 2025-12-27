package com.gdn.x.mta.distributiontask.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class PDTProductCombinedUpdateToSolrEventModel {
  //will be used for addition. Will be non null if addition is happening
  private PDTProductSolrAddDomainEventModel pdtProductSolrAddDomainEventModel;

  //will be used for solr updates. Will be non null if update is happening
  private PDTProductUpdateProductToSolrEventModel pdtProductUpdateProductToSolrEventModel;

  //will be used for deletion. Will be non null if deletion is happening
  private String productCode;
}
