package com.gdn.mta.bulk.models;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class GenerateQrCodeNotificationModel {
  String qrGenerationTypeEn;
  String qrGenerationTypeIn;
  String redirectionUrl;
  String merchantCode;
}
