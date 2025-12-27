package com.gdn.x.productcategorybase.controller.brand;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.BrandControllerErrorMessage;
import com.gdn.x.productcategorybase.BrandControllerPath;
import com.gdn.x.productcategorybase.dto.BrandDTO;
import com.gdn.x.productcategorybase.dto.BrandSummaryFilterDTO;
import com.gdn.x.productcategorybase.dto.brand.BrandNamesRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandNamesResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandPredefinedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandSummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandResponse;
import com.gdn.x.productcategorybase.dto.brand.UndeleteBrandRequest;
import com.gdn.x.productcategorybase.dto.brand.UndeleteBrandResponse;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandRequest;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandlogoPath;
import com.gdn.x.productcategorybase.dto.response.BrandSummaryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProtectedBrandResponse;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.repository.SolrBrandRepository;
import com.gdn.x.productcategorybase.service.brand.BrandService;
import com.gdn.x.productcategorybase.service.brand.BrandWipService;
import com.gdn.x.productcategorybase.util.ConverterUtil;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping(value = BrandControllerPath.BASE_PATH)
@Tag(name = "Brand", description = "Brand Service API")
public class BrandController {

  @Autowired
  private BrandService brandService;

  @Autowired
  private BrandWipService brandWipService;

  @Autowired
  private SolrBrandRepository solrBrandRepository;

  private static final Logger LOGGER = LoggerFactory.getLogger(BrandController.class);
  private static final int REQUEST_LIMIT = 500;
  private static final String VALID_CHARACTERS_REGEX = "[\\x20-\\x7E]+";
  private static final String PATH_SEPARATOR = "/";

  @RequestMapping(value = BrandControllerPath.CREATE, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "create brand", description = "create brand")
  
  public GdnRestSingleResponse<CreateBrandResponse> create(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody CreateBrandRequest request) throws Exception {
    this.validateCreate(request);
    Brand brand = this.generateCreateBrandRequestToBrand(request);
    String brandCode = this.brandService.create(brand);
    CreateBrandResponse createBrandResponse = this.generateCreateBrandResponse(brandCode);
    return new GdnRestSingleResponse<CreateBrandResponse>(createBrandResponse, requestId);
  }

  @RequestMapping(value = BrandControllerPath.UPDATE, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update brand", description = "update brand")
  
  public GdnRestSingleResponse<UpdateBrandlogoPath> update(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody UpdateBrandRequest request) throws Exception {
    this.validateUpdate(request);
    Brand brand = this.generateUpdateBrandRequestToBrand(request);
    UpdateBrandlogoPath brandlogoPath = this.brandService
        .update(brand, request.getBrandRequestCode(), request.getBrandLogo(), request.getProfileBanner(), username,
            request.getSkuCreationAllowedForAllSellers());
    brandService.deleteAllBrandCache(storeId, brand.getBrandName().toLowerCase(),
        brand.getBrandCode());
    return new GdnRestSingleResponse<>(brandlogoPath,requestId);
  }

  @RequestMapping(value = BrandControllerPath.DELETE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "delete brand", description = "delete brand")
  
  public GdnBaseRestResponse delete(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam String brandCode, @RequestParam(required = false) String brandDeletedReason) throws Exception {
    try {
      this.validateDelete(brandCode);
      String brandName = this.brandService.delete(brandCode, brandDeletedReason);
      brandService.deleteAllBrandCache(storeId, brandName, brandCode);
      return new GdnBaseRestResponse(requestId);
    } catch (ApplicationRuntimeException e) {
      return new GdnBaseRestResponse(e.getErrorMessage(), e.getErrorCodes().getCode(), false, requestId);
    } catch (Exception e) {
      return new GdnBaseRestResponse(null, null, false, requestId);
    }
  }

  @RequestMapping(value = BrandControllerPath.UNDELETE, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "undelete brand", description = "undelete brand")
  
  public GdnRestSingleResponse<UndeleteBrandResponse> undelete(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody UndeleteBrandRequest request) throws Exception {
    this.validateUndelete(request);
    Brand brand = this.generateUndeleteBrandRequestToBrand(request);
    Brand response = this.brandService.undelete(brand);
    solrBrandRepository.addBrandsToBrandCollectionSolr(
        Arrays.asList(ConverterUtil.generateSolrBrandModelApproveExistingBrand(response)));
    UndeleteBrandResponse undeleteBrandResponse = this.generateUndeleteBrandResponse(response.getBrandCode());
    brandService.deleteAllBrandCache(storeId, response.getBrandName(), response.getBrandCode());
    return new GdnRestSingleResponse<>(undeleteBrandResponse, requestId);
  }

  @RequestMapping(value = BrandControllerPath.FILTER_BRAND_CODE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter by brand code", description = "filter by brand code")
  
  public GdnRestSingleResponse<BrandResponse> filterByBrandCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String brandCode) throws Exception {
    log.info("Fetching brand response by brand code : {} ", brandCode);
    BrandResponse brandResponse = brandService.getBrandResponseByBrandCode(storeId, brandCode);
    return new GdnRestSingleResponse<>(brandResponse, requestId);
  }

  @RequestMapping(value = BrandControllerPath.GET_BRAND_NAMES_BY_CODES, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get names by brand codes", description = "get names by brand codes")
  
  public GdnRestListResponse<BrandNamesResponse> getBrandNamesByBrandCodes(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody BrandNamesRequest request) throws Exception {
    List<BrandNamesResponse> response = new ArrayList<>();
    String errorCode = null;
    String errorMessage = null;
    boolean success = false;
    try {
      if (CollectionUtils.isNotEmpty(request.getBrandCodes())) {
        if (request.getBrandCodes().size() < REQUEST_LIMIT) {
          List<BrandDTO> brandNamesDTOs = this.brandService.findBrandNamesByBrandCodes(request.getBrandCodes());
          //        response = brandNamesDTOs.stream()
          //            .map(e -> new BrandNamesResponse(e.getBrandCode(), e.getBrandName()))
          //            .collect(Collectors.toList());
          if (!CollectionUtils.isEmpty(brandNamesDTOs)) {
            for (BrandDTO brandDTO : brandNamesDTOs) {
              BrandNamesResponse brandResponse = new BrandNamesResponse();
              BeanUtils.copyProperties(brandDTO, brandResponse);
              response.add(brandResponse);
            }
          }
          success = true;
        } else {
          errorMessage = "Request should not exceed 500 brands," + ErrorCategory.VALIDATION.getMessage();
          errorCode = ErrorCategory.VALIDATION.getCode();
        }
      }
    } catch (Exception e) {
      LOGGER.error("Error fetching brand names " + e.getMessage(), e);
      errorCode = ErrorCategory.DATA_ACCESS.getCode();
      errorMessage = e.getMessage();
    }
    return new GdnRestListResponse<>(errorMessage, errorCode, success, response, null, requestId);
  }

  @RequestMapping(value = BrandControllerPath.FILTER_BRAND_NAME, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter by brand name", description = "filter by brand name")
  
  public GdnRestSingleResponse<BrandResponse> filterByBrandName(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String brandName,
      @RequestParam(defaultValue = "false") boolean markForDelete,
      @RequestParam(defaultValue = "false") boolean activeBrandsOnly) throws Exception {
    log.info("Fetching brand response by brand name : {} ", brandName);
    BrandResponse brandResponse = brandService.getBrandResponseByBrandName(storeId, brandName,
      markForDelete,activeBrandsOnly);
    return new GdnRestSingleResponse<>(brandResponse, requestId);
  }

  @RequestMapping(value = BrandControllerPath.FILTER_SUMMARY, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter summary", description = "filter summary")
  
  public GdnRestListResponse<BrandResponse> filterSummary(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "10") Integer size,
      @RequestBody BrandSummaryRequest request) throws Exception {
    Page<BrandResponse> brandResponses = this.findSummaryByFilter(request, storeId, page, size);
    return new GdnRestListResponse<>(brandResponses.getContent(),
        new PageMetaData(size, page, brandResponses.getTotalElements()), requestId);
  }

  @RequestMapping(value = BrandControllerPath.FILTER_SUMMARY_ORDER_BY_NAME, method =
      RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter summary", description = "filter summary")
  public GdnRestListResponse<BrandResponse> filterSummaryByName(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String brandName, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {

    Pageable pageable = PageRequest.of(page, size);
    Page<Brand> brands = this.brandService.findSummaryByName(brandName, pageable);
    Page<BrandResponse> brandResponses =
        new PageImpl<BrandResponse>(this.generateBrandResponses(brands), pageable, brands.getTotalElements());
    return new GdnRestListResponse<BrandResponse>(brandResponses.getContent(),
        new PageMetaData(size, page, brandResponses.getTotalElements()), requestId);
  }

  @RequestMapping(value = BrandControllerPath.FILTER_BRAND_BY_CODE_AND_STATUS, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter by brand code and status ", description = "filter by brand code and status")
  
  public GdnRestSingleResponse<BrandPredefinedAttributeValueResponse> filterByBrandCodeAndStatus(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @PathVariable("status") String status,
      @PathVariable("brandCode") String brandCode) throws Exception {
    try {
      BrandPredefinedAttributeValueResponse brandPredefinedValuesByNameAndState =
          this.brandService.getBrandPredefinedValueByCodeAndState(storeId, brandCode, status);
      return new GdnRestSingleResponse<>(null, null, true, brandPredefinedValuesByNameAndState, requestId);
    } catch (Exception e) {
      LOGGER.error("Error while fetching brand response with code :{}", brandCode, e.getMessage());
      return new GdnRestSingleResponse<>(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          false, null, requestId);
    }
  }

  private void validateCreate(CreateBrandRequest request) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getBrandName()),
        BrandControllerErrorMessage.BRAND_NAME_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getBrandDescription()),
        BrandControllerErrorMessage.BRAND_DESCRIPTION_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(containsValidCharacter(request.getBrandName()),
        BrandControllerErrorMessage.INVALID_BRAND_NAME_CHARACTERS);
  }

  private void validateUpdate(UpdateBrandRequest request) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getBrandCode()),
        BrandControllerErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getBrandName()),
        BrandControllerErrorMessage.BRAND_NAME_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getBrandDescription()),
        BrandControllerErrorMessage.BRAND_DESCRIPTION_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getBrandRequestCode()),
        BrandControllerErrorMessage.BRAND_REQUEST_CODE_MUST_NOT_BE_BLANK);
  }

  private void validateDelete(String brandCode) throws Exception {
    GdnPreconditions
        .checkArgument(StringUtils.isNotEmpty(brandCode), BrandControllerErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK);
  }

  private void validateUndelete(UndeleteBrandRequest request) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getBrandName()),
        BrandControllerErrorMessage.BRAND_NAME_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getBrandDescription()),
        BrandControllerErrorMessage.BRAND_DESCRIPTION_MUST_NOT_BE_BLANK);
  }

  private static boolean containsValidCharacter(String input) {
    return input.matches(VALID_CHARACTERS_REGEX);
  }

  private Brand generateCreateBrandRequestToBrand(CreateBrandRequest request) throws Exception {
    Brand brand = new Brand();
    BeanUtils.copyProperties(request, brand, new String[] {"brandDescription"});
    brand.setBrandDescription(request.getBrandDescription().getBytes());
    return brand;
  }

  private Brand generateUpdateBrandRequestToBrand(UpdateBrandRequest request) throws Exception {
    Brand brand = new Brand();
    BeanUtils.copyProperties(request, brand, new String[] {"brandDescription","skuCreationAllowedForAllSellers"});
    if (Objects.nonNull(request.getSkuCreationAllowedForAllSellers())) {
      brand.setSkuCreationAllowedForAllSellers(request.getSkuCreationAllowedForAllSellers());
    }
    brand.setBrandDescription(request.getBrandDescription().getBytes());
    return brand;
  }

  private Brand generateUndeleteBrandRequestToBrand(UndeleteBrandRequest request) throws Exception {
    Brand brand = new Brand();
    BeanUtils.copyProperties(request, brand, new String[] {"brandDescription"});
    brand.setBrandDescription(request.getBrandDescription().getBytes());
    return brand;
  }

  private CreateBrandResponse generateCreateBrandResponse(String brandCode) throws Exception {
    CreateBrandResponse createBrandResponse = new CreateBrandResponse();
    createBrandResponse.setBrandCode(brandCode);
    return createBrandResponse;
  }

  private UndeleteBrandResponse generateUndeleteBrandResponse(String brandCode) throws Exception {
    UndeleteBrandResponse undeleteBrandResponse = new UndeleteBrandResponse();
    undeleteBrandResponse.setBrandCode(brandCode);
    return undeleteBrandResponse;
  }

  private BrandResponse generateBrandResponse(Brand brand) {
    BrandResponse brandResponse = null;
    if (brand != null) {
      brandResponse = new BrandResponse();
      BeanUtils.copyProperties(brand, brandResponse,
          new String[] {"brandDescription", "brandLogoPath", "profileBannerPath"});
      brandResponse
          .setBrandDescription(brand.getBrandDescription() == null ? null : new String(brand.getBrandDescription()));
      if (StringUtils.isNotEmpty(brand.getBrandLogoPath())) {
        brandResponse
            .setBrandLogoPath(PATH_SEPARATOR + brand.getBrandCode() + PATH_SEPARATOR + brand.getBrandLogoPath());
      }
      if (StringUtils.isNotEmpty(brand.getProfileBannerPath())) {
        brandResponse.setProfileBannerPath(
            PATH_SEPARATOR + brand.getBrandCode() + PATH_SEPARATOR + brand.getProfileBannerPath());
      }
    }
    return brandResponse;
  }

  private List<BrandResponse> generateBrandResponses(Page<Brand> brands) {
    List<BrandResponse> brandResponses = new ArrayList<BrandResponse>();
    for (Brand brand : brands.getContent()) {
      BrandResponse brandResponse = this.generateBrandResponse(brand);
      brandResponses.add(brandResponse);
    }
    return brandResponses;
  }

  private Page<BrandResponse> findSummaryByFilter(BrandSummaryRequest request, String storeId, Integer page,
      Integer size) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    Page<Brand> brands = this.brandService.findSummaryByFilter(
        BrandSummaryFilterDTO.builder().brandName(request.getBrandName()).updatedDate(request.getUpdatedDate())
            .sortDirection(request.getSortDirection()).sortedBy(request.getSortedBy())
            .markForDelete(request.isMarkForDelete()).pageable(pageable).build(), storeId);
    return new PageImpl<>(this.generateBrandResponses(brands), pageable, brands.getTotalElements());
  }

  @RequestMapping(value = BrandControllerPath.GET_DEFAULT_BRANDS, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get default brands", description = "get default brands")
  
  public GdnRestListResponse<PredefinedAllowedAttributeValueResponse> getDefaultBrands(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username) throws Exception {
    LOGGER.info("fetching the default brands by requestId : {}", requestId);
    try {
      List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValues =
          this.brandService.getDefaultBrands(storeId);
      return new GdnRestListResponse<>(null, null, true, predefinedAllowedAttributeValues,
          new PageMetaData(0, 0, predefinedAllowedAttributeValues.size()), requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught in fetching the default brands for requestId : {}", requestId, e);
      return new GdnRestListResponse<>(null, null, false, null, null, requestId);
    }
  }

  @RequestMapping(value = BrandControllerPath.GET_VALID_BRAND_SUMMARY, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get valid brands", description = "get valid brands")
  
  public GdnRestListResponse<BrandSummaryResponse> getValidBrandSummary(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam int page, @RequestParam int size)
      throws Exception {
    LOGGER.info("fetching the valid brands. requestId : {}, page : {}, size : {} ", requestId, page, size);
    try {
      Page<BrandSummaryResponse> brandSummaryResponses =
          brandService.getBrandSummaryResponseForValidBrands(storeId, page, size);
      return new GdnRestListResponse<>(null, null, true, brandSummaryResponses.getContent(),
          new PageMetaData(size, page, brandSummaryResponses.getTotalElements()), requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught in fetching the valid brands for requestId : {}, page : {}, size : {} ", requestId,
          page, size, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    }
  }

  @RequestMapping(value = BrandControllerPath.PROTECTED_BRANDS, method = RequestMethod.GET, produces = {
    MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Get list of protected brands", description = "Get list of protected brands")
  
  public GdnRestListResponse<ProtectedBrandResponse> getProtectedBrandList(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam String username) {
    try {
      List<ProtectedBrandResponse> brandListResponses =
        this.brandService.getProtectedBrandList(storeId);
      return new GdnRestListResponse<>(null, null, true, brandListResponses,
        null, requestId);
    } catch (Exception e) {
      log.error("Error while fetching list of protected brands for requestId : {} error : {}",
        requestId, e.getMessage());
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, null, null, requestId);
    }
  }

}
