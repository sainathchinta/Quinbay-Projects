package com.gdn.mta.bulk.dto;

import java.util.ArrayList;
import java.util.List;

import com.gdn.mta.bulk.MerchantStatusType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class GenericTemplateDataReadDTO {
  private List<List<Object>> userInputRows = new ArrayList<>();
  private MerchantStatusType merchantStatusType;
  private byte[] fileByteData;
  private String merchantType;
  private boolean productBundlingEnabled;
  private String productBundlingEligibleMerchantTypes;
  private boolean genericFileHeaderValidationEn;
  private Boolean internationalMerchant;
  private List<Object> excelBahasaHeaderList = new ArrayList<>();
  private List<Object> excelEnglishHeaderList = new ArrayList<>();
  private String bulkGenericExcelVersion;
  private String excelFileName;
  private boolean bulkExcelVersioningEn;
  private List<Integer> failedExcelRows = new ArrayList<>();
  private boolean instoreEligible;
  private String bulkGenericInstoreExcelVersion;
}
