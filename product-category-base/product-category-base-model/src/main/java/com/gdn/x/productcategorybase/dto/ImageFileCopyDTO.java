package com.gdn.x.productcategorybase.dto;

import java.io.File;

import lombok.Data;

@Data
public class ImageFileCopyDTO {
  private File sourceFile;
  private String filename;
  byte[] imageDownloadFromGcsBytes;
  boolean imageDownloadedFromGcs;
}
