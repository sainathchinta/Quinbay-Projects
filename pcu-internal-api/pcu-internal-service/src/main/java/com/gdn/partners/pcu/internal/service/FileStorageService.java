package com.gdn.partners.pcu.internal.service;

import java.io.IOException;
import java.util.Map;

import com.gdn.partners.pcu.internal.service.model.UploadAttributeImageRequest;
import com.gdn.partners.pcu.internal.web.model.request.UploadImageRequest;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandRequest;

public interface FileStorageService {

  /**
   *
   * @param multipartFile
   * @param requestId
   * @param bulkInternalProcessType
   * @return
   * @throws Exception
   */
  String uploadFilePath(MultipartFile multipartFile, String requestId, String bulkInternalProcessType) throws Exception;

  /**
   * to get interanl download template file paths
   *
   * @return
   */
  Map<String, String> getInternalDownloadTemplateFilePaths();

  /**
   *
   * @param imageFileName
   * @param splitImageFilenameByDash
   * @param sourceDirectory
   * @return
   */
  int checkImageSize(String imageFileName, String[] splitImageFilenameByDash,
    StringBuilder sourceDirectory) throws IOException;

  /**
   * @param imageFileName
   * @return
   */
  int getImageSize(String imageFileName) throws IOException;

  /**
   *
   * @param brandCode Brand code of the brand auth doc
   * @param sellerCode Seller code of the brand auth doc
   * @param fileName Name of the file
   * @param multipartFile File to be uploaded
   * @return
   * @throws IOException
   */
  void uploadBrandAuthDoc(String fileName, String brandCode, String sellerCode,  byte[] multipartFile)
    throws Exception;

  /**
   *
   * @param fileName Name of the file
   * @param brandCode Brand code
   * @param sellerCode Seller code
   * @return
   */
  byte[] checkIfFileExisting(String fileName, String brandCode, String sellerCode);

  /**
   * upload brand files to GCS
   *
   * @param multipartFile File to be uploaded
   * @param locationPath Path for uploading the file
   * @throws Exception
   */
  String uploadBrandFileToGcs(MultipartFile multipartFile, String locationPath) throws Exception;

  /**
   * generating brandLogo file in GCS
   *
   * @param brandApproveRequest to get file data
   * @param brandLogo multipart file to be uploaded
   * @param brandRequestCode for a particular brand logo folder
   */
  void createBrandLogoFile(BrandApproveRequest brandApproveRequest, MultipartFile brandLogo, String brandRequestCode)
      throws Exception;

  /**
   * generating profileBanner file
   *
   * @param brandApproveRequest to get file data
   * @param profileBanner multipart file to be uploaded
   * @param brandRequestCode for a particular brand logo folder
   * @throws Exception
   */
  void createBrandProfileBannerFile(BrandApproveRequest brandApproveRequest, MultipartFile profileBanner,
      String brandRequestCode) throws Exception;

  /**
   * Profile Banner Approval
   *
   * @param brandApproveRequest to get file data
   * @param brandRequestCode    for a particular brand logo folder
   * @param profileBanner       file to be approved
   * @param brandCode           for a particular brand check in filestore
   * @throws Exception
   */
  void approveProfileBanner(BrandApproveRequest brandApproveRequest, String brandRequestCode,
      MultipartFile profileBanner, String brandCode) throws Exception;

  /**
   * Brand Logo Approval
   *
   * @param brandApproveRequest to get file data
   * @param brandRequestCode    for a particular brand logo folder
   * @param brandLogo           file to be approved
   * @param brandCode           for a particular brand check in filestore
   * @throws Exception
   */
  void approveBrandLogo(BrandApproveRequest brandApproveRequest, String brandRequestCode, MultipartFile brandLogo,
      String brandCode) throws Exception;

  /**
   * for updating brand files
   *
   * @param updateBrandRequest to get file data
   * @param brandLogo file to be updated
   * @param profileBanner file to be updated
   * @param brandRequestCode for a particular brand file folder
   * @throws Exception
   */
  void updateBrandFiles(UpdateBrandRequest updateBrandRequest, MultipartFile brandLogo, MultipartFile profileBanner,
      String brandRequestCode) throws Exception;

  /**
   * for deleting updates files from GCS
   *
   * @param locationPath path at which files will be deleted
   * @throws Exception
   */
  void deleteUpdatedBrandLogo(String locationPath, String brandRequestCode) throws Exception;

  /**
   *
   * @param brandCode
   * @param brandLogoPath
   * @param isBandApproved
   * @param isBrandLogo
   * @return
   */
  byte[] getBrandImage(String brandCode, String brandLogoPath, boolean isBandApproved, boolean isBrandLogo);

  /**
   * upload active attribute image to gcs
   *
   * @param request
   */
  String uploadAttributeImages(UploadAttributeImageRequest request);

  /**
   * upload active images to filestore or gcs
   *
   * @param request
   */
  void uploadActiveImages(UploadImageRequest request);

  /**
   * uploading file to gcs
   *
   * @param uploadImageRequest
   * @return
   */
  String uploadImage(UploadImageRequest uploadImageRequest) throws Exception;
}
