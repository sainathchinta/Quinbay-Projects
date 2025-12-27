package com.gdn.partners.pcu.internal.service.impl;

import java.io.File;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import com.gdn.partners.pcu.internal.service.BrandWipService;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.impl.helper.ImageHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.web.model.request.ApproveBrandWipWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BrandApprovalResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandRejectionWebResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectionInfoResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistoryResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistorySummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipSummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandResponse;

@Service
public class BrandWipServiceImpl implements BrandWipService {

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private SystemParameterProperties systemParameterProperties;

  @Autowired
  private FileStorageService fileStorageService;

  @Override
  public GdnRestSingleResponse<BrandWipResponse> getBrandWipDetail(String brandRequestCode) {
    GdnRestSingleResponse<BrandWipResponse> response = pcbFeign.getBrandWipDetail(brandRequestCode);
    ResponseHelper.validateResponse(response);
    return response;
  }

  @Override
  public GdnRestListResponse<BrandWipHistoryResponse> getBrandWipHistory(
      BrandWipHistorySummaryRequest brandWipHistorySummaryRequest, int page, int size) {
    GdnRestListResponse<BrandWipHistoryResponse> response =
        pcbFeign.getBrandWipHistory(brandWipHistorySummaryRequest, page, size);
    ResponseHelper.validateResponse(response);
    return response;
  }

  @Override
  public GdnRestListResponse<BrandWipResponse> getBrandWipList(BrandWipSummaryRequest brandWipSummaryRequest, int page, int size) {
    GdnRestListResponse<BrandWipResponse> response = pcbFeign.getBrandWipList(brandWipSummaryRequest, page, size);
    ResponseHelper.validateResponse(response);
    return response;
  }

  @Override
  public BrandApprovalResponse approveBrand(ApproveBrandWipWebRequest approveBrandWipWebRequest, MultipartFile brandLogo,
      MultipartFile profileBanner) throws Exception {
    ImageHelper.validate_images(brandLogo, profileBanner);
    BrandApproveRequest brandApproveRequest = RequestHelper
        .approveBrandWipWebRequestToBrandApproveRequest(approveBrandWipWebRequest, brandLogo, profileBanner);
    GdnRestSingleResponse<CreateBrandResponse> brandApproveResponse =
        pcbFeign.approveBrand(brandApproveRequest.getBrandRequestCode(), brandApproveRequest);
    ResponseHelper.validateResponse(brandApproveResponse);
    brandApproveRequest.setBrandLogoPath(brandApproveResponse.getValue().getBrandLogoPath());
    brandApproveRequest.setProfileBannerPath(brandApproveResponse.getValue().getProfileBannerPath());
    setBrandLogoAndBanner(brandLogo, profileBanner, brandApproveRequest, brandApproveResponse);
    return new BrandApprovalResponse(brandApproveResponse.getValue().getBrandCode(),
        brandApproveResponse.getValue().getBrandName());
  }

  @Override
  public BrandRejectionWebResponse getBrandRejectionReasonByBrandRequestCode(String brandRequestCode) {
    GdnRestSingleResponse<BrandRejectionInfoResponse> response =
        pcbFeign.getBrandRejectionReasonByBrandRequestCode(brandRequestCode);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toBrandRejectionWebResponse(response.getValue());
  }

  @Override
  public GdnBaseRestResponse update(ApproveBrandWipWebRequest approveBrandWipWebRequest, MultipartFile brandLogo,
      MultipartFile profileBanner) throws Exception {
    ImageHelper.validate_images(brandLogo, profileBanner);
    BrandApproveRequest brandApproveRequest =
        RequestHelper.toBrandApproveRequest(approveBrandWipWebRequest, brandLogo, profileBanner);
    GdnBaseRestResponse response = pcbFeign.update(brandApproveRequest);
    ResponseHelper.validateResponse(response);
    createBrandLogoAndProfileBanner(brandApproveRequest, brandLogo, profileBanner);
    return response;
  }

  private void setBrandLogoAndBanner(MultipartFile brandLogo, MultipartFile profileBanner,
      BrandApproveRequest brandApproveRequest, GdnRestSingleResponse<CreateBrandResponse> response) throws Exception {
    createBrandLogoAndProfileBanner(brandApproveRequest, brandLogo, profileBanner);
    fileStorageService.approveBrandLogo(brandApproveRequest, brandApproveRequest.getBrandRequestCode(), brandLogo,
        response.getValue().getBrandCode());
    fileStorageService.approveProfileBanner(brandApproveRequest, brandApproveRequest.getBrandRequestCode(),
        profileBanner, response.getValue().getBrandCode());
  }

  private void createBrandLogoAndProfileBanner(BrandApproveRequest brandApproveRequest, MultipartFile brandLogo,
      MultipartFile profileBanner) throws Exception {
    fileStorageService.createBrandLogoFile(brandApproveRequest, brandLogo, brandApproveRequest.getBrandRequestCode());
    fileStorageService.createBrandProfileBannerFile(brandApproveRequest, profileBanner,
        brandApproveRequest.getBrandRequestCode());
  }

  @Override
  public String rejectBrand(BrandRejectRequest brandRejectRequest) {
    GdnRestSingleResponse<BrandWipResponse> response =
        pcbFeign.rejectBrand(brandRejectRequest.getBrandRequestCode(), brandRejectRequest);
    ResponseHelper.validateResponse(response);
    this.deleteBrandImageFolder(response.getValue());
    return response.getValue().getBrandCode();
  }

  /**
   * Delete the brand logo and profile banner source folders
   *
   * @param brandWipResponse
   */
  private void deleteBrandImageFolder(BrandWipResponse brandWipResponse) {
    String logoLocationPath =
        new StringBuilder(systemParameterProperties.getDirectoryBrandLogoSource()).append(Constants.SLASH)
            .append(brandWipResponse.getBrandCode()).append(Constants.SLASH).append(brandWipResponse.getBrandLogoPath())
            .toString();
    this.deleteFile(logoLocationPath);
    String profileBannerLocationPath =
        new StringBuilder(systemParameterProperties.getDirectoryProfileBannerSource()).append(Constants.SLASH)
            .append(brandWipResponse.getBrandCode()).append(Constants.SLASH)
            .append(brandWipResponse.getProfileBannerPath()).toString();
    this.deleteFile(profileBannerLocationPath);
  }

  private void deleteFile(String locationPath) {
    File file = new File(locationPath);
    if (file.exists()) {
      file.delete();
    }
  }
}
