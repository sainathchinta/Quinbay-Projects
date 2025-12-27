package com.gdn.mta.bulk.dto;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkInternalProcessDataGenerationDTO {

  private int brandAuthEndYear;
  private int bulkPriceUpdateMaxRows;
  private int bulkProductTypeTaggingMaxRows;
  private int minPriceValue;
  private int bulkRebateMaxRows;
  private int bulkSkuLevelRebateMaxRows;
  private int bulkPriceUpdateNewMaxRows;
  private boolean noRowsDetectedForPriceUpdate;
  private String storeId;
  private String userName;
  private Set<String> iprValidActions = new HashSet<>();
  private Set<String> iprSource = new HashSet<>();
  private Set<String> iprViolationTypes = new HashSet<>();
  private Set<String> iprReasons = new HashSet<>();
  private List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
  private Map<String, List<String>> reviewers = new HashMap<>();
  private BulkInternalProcess bulkInternalProcess;
}
