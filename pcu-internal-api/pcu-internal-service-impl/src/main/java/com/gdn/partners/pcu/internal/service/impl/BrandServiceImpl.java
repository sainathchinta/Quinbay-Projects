package com.gdn.partners.pcu.internal.service.impl;

import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.core.exception.ApplicationException;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import com.gdn.partners.pcu.internal.service.BrandService;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.internal.service.impl.helper.ImageHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.web.model.request.BrandDeleteWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BrandLogo;
import com.gdn.partners.pcu.internal.web.model.response.BrandWebResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandRequest;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandlogoPath;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BrandServiceImpl implements BrandService {

  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private SystemParameterProperties systemParameterProperties;

  @Autowired
  private FileStorageService fileStorageService;

  private static final String SLASH = "/";



  @Override
  public void deleteBrand(String brandCode, BrandDeleteWebRequest brandDeleteWebRequest) {
    GdnRestSimpleResponse<Long> productSkuCountMappedToBrandResponse = pbpFeign
        .getProductsCountByBrandName(brandDeleteWebRequest.getBrandName());
    ResponseHelper.validateResponse(productSkuCountMappedToBrandResponse);
    Long productSkuCountMappedToBrand = productSkuCountMappedToBrandResponse.getValue();
    if(productSkuCountMappedToBrand == 0) {
      GdnBaseRestResponse deleteBrandResponse =
          pcbFeign.deleteBrand(brandCode, brandDeleteWebRequest.getBrandDeletedReason());
      ResponseHelper.validateResponse(deleteBrandResponse);
    } else {
      throw new InvalidStateException(ErrorMessages.BRAND_HAS_PRODUCTS_MAPPED);
    }
  }

  @Override
  public BrandWebResponse getBrandDetail(String brandCode) {
    GdnRestSingleResponse<BrandResponse> response = pcbFeign.getBrandDetail(brandCode);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toBrandWebResponse(response.getValue());
  }

  @Override
  public BrandLogo filterBrandLogoByBrandCode(String brandCode, boolean isBrandLogo) throws Exception{
    GdnRestSingleResponse<BrandResponse> response = pcbFeign.filterByBrandCode(brandCode);
    ResponseHelper.validateBrandResponse(response);
    BrandResponse brandResponse = response.getValue();
    if(Objects.isNull(brandResponse)) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND.toString(), "Brand with brand code " + brandCode
          + " is not found");
    }
    return this.findBrandLogo(brandResponse, isBrandLogo);
  }

  @Override
  public BrandLogo filterBrandLogoByBrandRequestCode(String brandRequestCode, boolean isBrandLogo) throws Exception{
    GdnRestSingleResponse<BrandWipResponse> response = pcbFeign.filterByBrandRequestCode(brandRequestCode);
    ResponseHelper.validateBrandResponse(response);
    BrandWipResponse brandWipResponse = response.getValue();
    if(Objects.isNull(brandWipResponse)) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND.toString(),
          "Brand Wip with brand request code " + brandRequestCode + " is not found");
    }
    return this.findBrandLogo(brandWipResponse, isBrandLogo);
  }

  private BrandLogo findBrandLogo(BrandResponse brandResponse, boolean isBrandLogo) throws Exception {
    String fileType;
    String imagePath;
    if (isBrandLogo) {
      fileType =
          brandResponse.getBrandLogoPath().substring(brandResponse.getBrandLogoPath().lastIndexOf(Constants.DOT) + 1);
      imagePath = brandResponse.getBrandLogoPath();
    } else {
      fileType = brandResponse.getProfileBannerPath()
          .substring(brandResponse.getProfileBannerPath().lastIndexOf(Constants.DOT) + 1);
      imagePath = brandResponse.getProfileBannerPath();
    }
    BrandLogo brandLogo = new BrandLogo();
    brandLogo.setBrandLogoData(
        fileStorageService.getBrandImage(brandResponse.getBrandCode(), imagePath, true, isBrandLogo));
    brandLogo.setBrandLogoPath(
        brandResponse.getBrandLogoPath().substring(brandResponse.getBrandLogoPath().lastIndexOf(Constants.SLASH) + 1));
    brandLogo.setBrandLogoFileType(fileType);
    return brandLogo;
  }

  private BrandLogo findBrandLogo(BrandWipResponse brandWip, boolean isBrandLogo) throws Exception {
    String fileType;
    String imagePath;
    String requiredCode;
    boolean isBrandApproved = false;
    if (isBrandLogo) {
      fileType = brandWip.getBrandLogoPath().substring(brandWip.getBrandLogoPath().lastIndexOf(Constants.DOT) + 1);
      imagePath = brandWip.getBrandLogoPath();
      if (BrandWipState.APPROVED.getDescription().equals(brandWip.getState())) {
        requiredCode = brandWip.getBrandCode();
        isBrandApproved = true;
      } else {
        requiredCode = brandWip.getBrandRequestCode();
      }
    } else {
      fileType = brandWip.getBrandLogoPath().substring(brandWip.getProfileBannerPath().lastIndexOf(Constants.DOT) + 1);
      imagePath = brandWip.getProfileBannerPath();
      if (BrandWipState.APPROVED.getDescription().equals(brandWip.getState())) {
        requiredCode = brandWip.getBrandCode();
        isBrandApproved = true;
      } else {
        requiredCode = brandWip.getBrandRequestCode();
      }
    }
    BrandLogo brandWipLogo = new BrandLogo();
    brandWipLogo.setBrandLogoData(
        fileStorageService.getBrandImage(requiredCode, imagePath, isBrandApproved, isBrandLogo));
    brandWipLogo.setBrandLogoPath(brandWip.getBrandLogoPath());
    brandWipLogo.setBrandLogoFileType(fileType);
    return brandWipLogo;
  }

  @Override
  public void updateBrand(UpdateBrandRequest request, MultipartFile brandLogo,
    MultipartFile profileBanner) throws Exception {
    ImageHelper.validate_images(brandLogo, profileBanner);
    GdnRestSingleResponse<BrandWipResponse> brandWipResponseGdnRestSingleResponse =
      pcbFeign.getBrandWipByBrandCode(request.getBrandCode());
    ResponseHelper.validateResponse(brandWipResponseGdnRestSingleResponse);
    BrandWipResponse savedBrandWip = brandWipResponseGdnRestSingleResponse.getValue();
    String brandRequestCode = savedBrandWip.getBrandRequestCode();
    String brandLogoPath = null;
    if (StringUtils.isNotEmpty(request.getBrandLogoPath())) {
      brandLogoPath = request.getBrandLogoPath().substring(request.getBrandLogoPath().lastIndexOf(SLASH) + 1);
    }
    String profileBannerPath = null;
    if (StringUtils.isNotEmpty(request.getProfileBannerPath())) {
      profileBannerPath = request.getProfileBannerPath().substring(request.getProfileBannerPath().lastIndexOf("/") + 1);
    }
    setUpdateBrandRequest(request, brandLogoPath, profileBannerPath, brandRequestCode, brandLogo, profileBanner);
    GdnRestSingleResponse<UpdateBrandlogoPath> updateBrandlogoPath = pcbFeign.updateBrand(request);
    ResponseHelper.validateResponse(updateBrandlogoPath);
    if (Objects.nonNull(brandLogo)) {
      fileStorageService.deleteUpdatedBrandLogo(updateBrandlogoPath.getValue().getBrandLogoPath(),
        request.getBrandCode());
      fileStorageService.updateBrandFiles(request, brandLogo, profileBanner,
        request.getBrandCode());
    }
  }

  private void setUpdateBrandRequest(UpdateBrandRequest request, String brandLogoPath, String profileBannerPath,
      String brandRequestCode, MultipartFile brandLogo, MultipartFile profileBanner) {
    request.setBrandLogoPath(brandLogoPath);
    request.setProfileBannerPath(profileBannerPath);
    request.setBrandRequestCode(brandRequestCode);
    if (Objects.nonNull(brandLogo) && !brandLogo.isEmpty()) {
      request.setBrandLogo(brandLogo.getOriginalFilename());
      request.setBrandLogoPath(RequestHelper.generatePath(request.getBrandName(), brandLogo));
    }
    if (Objects.nonNull(profileBanner) && !profileBanner.isEmpty()) {
      request.setProfileBanner(profileBanner.getOriginalFilename());
      request
          .setProfileBannerPath(RequestHelper.generatePath(request.getBrandName(), profileBanner));
    }
  }
}
