package com.gdn.partners.pcu.external.service;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.service.model.request.UploadAttributeImageRequest;
import com.gdn.partners.pcu.external.service.model.request.XgpImageScaleRequest;
import com.gdn.partners.pcu.external.web.model.request.ProcessFileType;
import com.gdn.partners.pcu.external.web.model.request.SignedUrlRequest;
import com.gdn.partners.pcu.external.web.model.response.FilePath;
import com.gdn.partners.pcu.external.web.model.response.SignedUrlResponse;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import com.gdn.partners.pcu.external.service.model.request.UploadImageRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipRequest;

public interface FileStorageService {

  /**
   *
   * @param username
   * @param filenames
   * @return
   * @throws IOException
   */
  List<MultipartFile> downloadMultiPartFile(String username, List<String> filenames)
    throws Exception;

  /**
   *
   * @param multipartFile
   * @param username
   */
  String uploadFileToGcs(MultipartFile multipartFile, String username) throws Exception;

  /**
   * 
   * @param multipartFile
   * @param bulkProcessCode
   * @return
   * @throws Exception
   */
  String uploadSubjectToVatFile(MultipartFile multipartFile, String bulkProcessCode) throws Exception;

  /**
   * uploading file to gcs
   *
   * @param uploadImageRequest
   * @return
   */
  String uploadImage(UploadImageRequest uploadImageRequest) throws Exception;

  /**
   * get path prefix
   *
   * @return
   */
  String getImagePathPrefix();

  /**
   * download file
   *
   * @param fileName
   * @return
   * @throws Exception
   */
  byte[] downloadFile(String fileName) throws Exception;

  /**
   *to get exteranl download template file paths
   *
   * @return
   */
  Map<String, String> getExternalDownloadTemplateFilePaths();

  /**
   *
   * @param fileName
   * @return
   * @throws IOException
   */
  byte[] getImageContent(String fileName) throws Exception;

  /**
   *
   * @param imagePath
   * @return
   * @throws Exception
   */
  byte[] getImageFromImagePath(String imagePath) throws Exception;

  /**
   * upload active images to filestore or gcs
   * @param request
   */
  void uploadActiveImages(UploadImageRequest request);

  /**
   * upload active attribute image to gcs
   *
   * @param request
   */
  String uploadAttributeImages(UploadAttributeImageRequest request);

  /**
   *
   * @param productCode
   * @param filename
   * @return
   */
  String generatePath(String productCode, String filename);

  /**
   * upload brand files to GCS
   *
   * @param multipartFile file to be uploaded
   * @param locationPath location for upload
   * @throws Exception
   */
  String uploadBrandFileToGcs(MultipartFile multipartFile, String locationPath) throws Exception;

  /**
   * generating brandLogo file
   *
   * @param createBrandWipRequest to get file data
   * @param brandRequestCode for a particular brand logo folder
   * @param brandLogo file to be uploaded
   */
  void createBrandLogoFile(CreateBrandWipRequest createBrandWipRequest, MultipartFile brandLogo, String brandRequestCode)
      throws Exception;

  /**
   * generating profileBanner file
   *
   * @param createBrandWipRequest to get file data
   * @param profileBanner file to be uploaded
   * @param brandRequestCode for a particular profile banner folder
   * @throws Exception
   */
  void createBrandProfileBannerFile(CreateBrandWipRequest createBrandWipRequest, MultipartFile profileBanner,
      String brandRequestCode) throws Exception;

  /**
   *upload evidence file to GCS
   *
   * @param multipartFile
   * @param productSku
   * @param businessPartnerCode
   */
  String uploadEvidenceFileToGcs(MultipartFile multipartFile, String productSku,
      String businessPartnerCode) throws Exception;

  /**
   *
   * @param multipartFile
   * @param keyword
   * @param processFileType
   * @param businessPartnerCode
   * @return filepath
   * @throws Exception
   */
  FilePath processFileUpload(MultipartFile multipartFile, String keyword, String processFileType,
      String businessPartnerCode) throws Exception;

  /**
   * Generates a signed URL for uploading files to Google Cloud Storage.
   *
   * @param signedUrlRequest the request containing file name and process type
   * @return a response containing the signed URL and other metadata
   */
  SignedUrlResponse generateSignedUrl(SignedUrlRequest signedUrlRequest);

  /**
   * Generate XgpImageScaleRequest based on the UploadImageRequest.
   *
   * @param uploadImageRequest
   * @return
   * @throws IOException
   */
  XgpImageScaleRequest generateXgpImageScaleRequest(UploadImageRequest uploadImageRequest)
      throws IOException;
}
