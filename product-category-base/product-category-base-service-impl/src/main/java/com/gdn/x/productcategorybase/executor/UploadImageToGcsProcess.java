package com.gdn.x.productcategorybase.executor;

import java.util.concurrent.Callable;

import com.gdn.x.productcategorybase.service.FileStorageService;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class UploadImageToGcsProcess implements Callable<Boolean> {
  private String fileStorePath;
  private String gcsPath;
  private FileStorageService fileStorageService;

  @Override
  public Boolean call() throws Exception {
    fileStorageService.uploadFileFromGfsToGcs(fileStorePath, gcsPath);
    return true;
  }
}
