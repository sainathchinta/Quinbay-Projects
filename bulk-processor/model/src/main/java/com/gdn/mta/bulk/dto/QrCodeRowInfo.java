package com.gdn.mta.bulk.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class QrCodeRowInfo {
  private Boolean isDarkTheme;
  private Integer qrPerPage;
  private String templateSize;
  private String qrGenerationType;
  private String merchantCode;
  private String merchantName;
  private boolean allStores;
  private boolean bulkExcelUpload;
  private boolean printPrice;
  private List<QrCodeRowItemInfo> rowItems = new ArrayList<>();
}
