package com.gdn.partners.pcu.external.service.impl;

import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.service.BrandWipService;
import com.gdn.partners.pcu.external.service.FileStorageService;
import com.gdn.partners.pcu.external.service.impl.exception.DuplicateEntryException;
import com.gdn.partners.pcu.external.service.impl.helper.ImageValidator;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipResponse;

@Service
public class BrandWipServiceBean implements BrandWipService {

  private static final String INTERNAL = "INTERNAL";
  private static final String LOGO_EXT = "-logo.";
  private static final String BANNER_EXT = "-banner.";

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private FileStorageService fileStorageService;


  @Override
  public GdnRestSingleResponse<CreateBrandWipResponse> create(CreateBrandWipRequest createBrandWipRequest,
      MultipartFile brandLogo, MultipartFile profileBanner) throws Exception {
    RequestHelper.validateRequest(createBrandWipRequest);
    this.validate_images(brandLogo, profileBanner);
    this.checkBrand(createBrandWipRequest.getBrandName());
    createBrandWipRequest.setBusinessPartnerCode(mandatoryParameterHelper.getBusinessPartnerCode());
    createBrandWipRequest.setBusinessPartnerName(mandatoryParameterHelper.getBusinessPartnerCode());
    if (StringUtils.isEmpty(createBrandWipRequest.getBusinessPartnerCode()) || StringUtils.isEmpty(createBrandWipRequest.getBusinessPartnerName())) {
      createBrandWipRequest.setBusinessPartnerCode(INTERNAL);
      createBrandWipRequest.setBusinessPartnerName(INTERNAL);
    } else {
      createBrandWipRequest.setProtectedBrand(false);
    }
    this.checkBrandWip(createBrandWipRequest.getBrandName(), createBrandWipRequest.getBusinessPartnerCode());
    if (Objects.nonNull(brandLogo) && !brandLogo.isEmpty()) {
      createBrandWipRequest
          .setBrandLogoPath(RequestHelper.generatePath(createBrandWipRequest.getBrandName(), brandLogo));
    }
    if (Objects.nonNull(profileBanner) && !profileBanner.isEmpty()) {
      createBrandWipRequest.setProfileBannerPath(
          RequestHelper.generatePath(createBrandWipRequest.getBrandName(), profileBanner));
    }
    GdnRestSingleResponse<CreateBrandWipResponse> response =
        this.pcbFeign.create(createBrandWipRequest);
    if (Objects.nonNull(brandLogo) && !brandLogo.isEmpty()) {
      fileStorageService.createBrandLogoFile(createBrandWipRequest, brandLogo,
          response.getValue().getBrandRequestCode());
    }
    if (Objects.nonNull(profileBanner) && !profileBanner.isEmpty()) {
      fileStorageService.createBrandProfileBannerFile(createBrandWipRequest, profileBanner,
          response.getValue().getBrandRequestCode());
    }
    ResponseHelper.validateResponse(response);
    return response;
  }

  @Override
  public BrandResponse findByName(String name) {
    GdnRestSingleResponse<BrandResponse> response = this.pcbFeign.filterByBrandName(name, true);
    ResponseHelper.validateBrandResponse(response);
    return response.getValue();
  }


  private void validate_images(MultipartFile brandLogo, MultipartFile profileBanner) throws Exception{
    if (Objects.nonNull(brandLogo) && !brandLogo.isEmpty()) {
      ImageValidator.validateImages(brandLogo.getBytes());
    }
    if (Objects.nonNull(profileBanner) && !profileBanner.isEmpty()) {
      ImageValidator.validateImages(profileBanner.getBytes());
    }
  }

  private BrandWipResponse findByNameAndBusinessPartnerCode(String brandName, String businessPartnerCode) {
    GdnRestSingleResponse<BrandWipResponse> response =
        this.pcbFeign.findByBrandNameAndBusinessPartnerCode(brandName, businessPartnerCode);
    ResponseHelper.validateBrandResponse(response);
    return response.getValue();
  }

  private void checkBrand(String brandName) {
    BrandResponse brand = this.findByName(brandName);
    if (Objects.nonNull(brand)) {
      throw new DuplicateEntryException("Brand Already Exists");
    }
  }

  private void checkBrandWip(String brandName, String businessPartnerCode) {
    BrandWipResponse brandWipResponse = this.findByNameAndBusinessPartnerCode(brandName, businessPartnerCode);
    if (Objects.nonNull(brandWipResponse)) {
      throw new DuplicateEntryException("Brand Wip Already Exists");
    }
  }
}
