package com.gdn.mta.bulk.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class MediumImageUploadRequest {
  private String imagePath;
}
