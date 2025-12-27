package com.gdn.mta.bulk.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.dto.RecatProcessSummaryResponse;
import com.gdn.mta.bulk.entity.BulkProcessImage;
import com.gdn.mta.bulk.entity.BulkProcessVideo;
import com.gdn.mta.bulk.entity.RecatProcess;
import com.gdn.mta.bulk.models.L3InfoUpdateChangeType;
import com.gdn.mta.bulk.models.ProductL3Response;
import com.gdn.mta.bulk.models.ProductMasterDataEditRequest;
import com.gdn.mta.bulk.models.download.responsedata.BrandAuthResponse;
import com.gdn.mta.bulk.request.BulkBasicInfoUpdateRequest;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterResponse;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class ResponseHelperTest {

  private static final String RECAT_REQUEST_CODE = "recatRequestCode";
  private static final String STATUS = "status";
  private static final int PRODUCT_COUNT = 10;
  private static final String CREATED_BY = "createdBy";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String MERCHANT_NAME = "merchantName";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_NAME = "merchantName";
  private static final Date AUTH_START_DATE = new Date(1689645885000L);
  private static final Date AUTH_END_DATE = new Date(1689645885000L);

  private RecatProcess recatProcess = new RecatProcess();

  @BeforeEach
  public void init() {
    recatProcess.setRecatRequestCode(RECAT_REQUEST_CODE);
    recatProcess.setCreatedBy(CREATED_BY);
    recatProcess.setTotalCount(PRODUCT_COUNT);
    recatProcess.setStatus(STATUS);
    recatProcess.setScheduledTime(new Date());
  }

  @Test
  public void toRecatProcessSummaryResponseListTest() {
    List<RecatProcessSummaryResponse> response =
        ResponseHelper.toRecatProcessSummaryResponseList(Arrays.asList(recatProcess));
    assertEquals(RECAT_REQUEST_CODE, response.get(0).getRecatRequestCode());
    assertEquals(STATUS, response.get(0).getStatus());
    assertEquals(CREATED_BY, response.get(0).getInitiator());
    assertEquals(PRODUCT_COUNT, response.get(0).getProductCount());
  }

  @Test
  public void toRecatProcessSummaryResponseList_nullProductCountTest() {
    recatProcess.setTotalCount(null);
    List<RecatProcessSummaryResponse> response =
        ResponseHelper.toRecatProcessSummaryResponseList(Arrays.asList(recatProcess));
    assertEquals(RECAT_REQUEST_CODE, response.get(0).getRecatRequestCode());
    assertEquals(STATUS, response.get(0).getStatus());
    assertEquals(CREATED_BY, response.get(0).getInitiator());
    assertEquals(0, response.get(0).getProductCount());
  }

  @Test
  public void validateResponseTest() {
    Assertions.assertThrows(RuntimeException.class, () -> ResponseHelper.validateResponse(null));
  }

  @Test
  public void validateResponse_successFalseTest() {
    Assertions.assertThrows(RuntimeException.class, () -> ResponseHelper.validateResponse(
        new GdnRestSingleResponse<>(null, null, false, null, null)));
  }

  @Test
  public void validateResponse_successTrueTest() {
    ResponseHelper.validateResponse(new GdnRestSingleResponse<>(null, null, true, null, null));
  }

  @Test
  public void setBrandAuthResponseTest() {
    Map<String, String> sellerCodeNameMap = Map.of(MERCHANT_CODE, MERCHANT_NAME);
    BrandAuthFilterResponse brandAuthFilterResponse = new BrandAuthFilterResponse();
    brandAuthFilterResponse.setBrandCode(BRAND_CODE);
    brandAuthFilterResponse.setBrandName(BRAND_NAME);
    brandAuthFilterResponse.setSellerCode(MERCHANT_CODE);
    brandAuthFilterResponse.setAuthStartDate(AUTH_START_DATE);
    brandAuthFilterResponse.setAuthEndDate(AUTH_END_DATE);
    List<BrandAuthResponse> brandAuthResponseList = new ArrayList<>();

    ResponseHelper.setBrandAuthResponse(brandAuthResponseList, List.of(brandAuthFilterResponse),
      sellerCodeNameMap, new SimpleDateFormat("dd/MM/yyyy"));

    assertEquals(BRAND_CODE, brandAuthResponseList.get(0).getBrandCode());
    assertEquals(BRAND_NAME, brandAuthResponseList.get(0).getBrandName());
    assertEquals(MERCHANT_CODE, brandAuthResponseList.get(0).getSellerCode());
    assertEquals(MERCHANT_NAME, brandAuthResponseList.get(0).getSellerName());
    assertEquals("18/07/2023", brandAuthResponseList.get(0).getAuthStartDate());
    assertEquals("18/07/2023", brandAuthResponseList.get(0).getAuthEndDate());
  }

  @Test
  public void setBrandAuthResponseWithNullCodeTest() {
    BrandAuthFilterResponse brandAuthFilterResponse = new BrandAuthFilterResponse();
    brandAuthFilterResponse.setBrandCode(BRAND_CODE);
    brandAuthFilterResponse.setBrandName(BRAND_NAME);
    brandAuthFilterResponse.setSellerCode(MERCHANT_CODE);
    brandAuthFilterResponse.setAuthStartDate(AUTH_START_DATE);
    brandAuthFilterResponse.setAuthEndDate(AUTH_END_DATE);
    List<BrandAuthResponse> brandAuthResponseList = new ArrayList<>();

    ResponseHelper.setBrandAuthResponse(brandAuthResponseList, Arrays.asList(brandAuthFilterResponse),
      new HashMap<>(), new SimpleDateFormat("dd/MM/yyyy"));

    assertEquals(BRAND_CODE, brandAuthResponseList.get(0).getBrandCode());
    assertEquals(BRAND_NAME, brandAuthResponseList.get(0).getBrandName());
    assertEquals("18/07/2023", brandAuthResponseList.get(0).getAuthStartDate());
    assertEquals("18/07/2023", brandAuthResponseList.get(0).getAuthEndDate());
  }

  @Test
  public void setBrandAuthResponseWithNullNameTest() {
    HashMap<String, String> codeNameMap = new HashMap<>();
    codeNameMap.put(null, MERCHANT_NAME);
    BrandAuthFilterResponse brandAuthFilterResponse = new BrandAuthFilterResponse();
    brandAuthFilterResponse.setBrandCode(BRAND_CODE);
    brandAuthFilterResponse.setBrandName(BRAND_NAME);
    brandAuthFilterResponse.setSellerCode(MERCHANT_CODE);
    brandAuthFilterResponse.setAuthStartDate(AUTH_START_DATE);
    brandAuthFilterResponse.setAuthEndDate(AUTH_END_DATE);
    List<BrandAuthResponse> brandAuthResponseList = new ArrayList<>();

    ResponseHelper.setBrandAuthResponse(brandAuthResponseList,
      Arrays.asList(brandAuthFilterResponse), codeNameMap, new SimpleDateFormat("dd/MM/yyyy"));

    assertEquals(BRAND_CODE, brandAuthResponseList.get(0).getBrandCode());
    assertEquals(BRAND_NAME, brandAuthResponseList.get(0).getBrandName());
    assertEquals("18/07/2023", brandAuthResponseList.get(0).getAuthStartDate());
    assertEquals("18/07/2023", brandAuthResponseList.get(0).getAuthEndDate());
  }

  @Test
  void testGetChangeTypes_AllFieldsChanged_ShouldReturnAllChangeTypes() {
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    masterDataProduct.setProductName("Old Product");
    masterDataProduct.setDescription("Old Description");

    ProductL3Response l3Response = new ProductL3Response();
    l3Response.setMasterDataProduct(masterDataProduct);
    l3Response.setOff2OnChannelActive(false);
    l3Response.setProductType(ProductType.BIG_PRODUCT);
    l3Response.setLength(1.0);
    l3Response.setWidth(1.0);
    l3Response.setHeight(1.0);
    l3Response.setWeight(1.0);
    l3Response.setShippingWeight(1.0);
    l3Response.setSizeChartCode("oldSize");
    l3Response.setVideoUrl("https://old.video");
    l3Response.setUrl("https://old.youtube");

    BulkBasicInfoUpdateRequest request = BulkBasicInfoUpdateRequest.builder()
        .productName("New Product")
        .inStore(true)
        .description("New Description")
        .shippingType("Produk non-fisik") // => code 1, different from 2
        .length(2.0)
        .width(2.0)
        .height(2.0)
        .weight(2.0)
        .shippingWeight(2.0)
        .sizeChartCode("newSize")
        .videoUrl("https://new.youtube")
        .youtubeUrl(true)
        .imagesUpdated(true)
        .build();

    Set<L3InfoUpdateChangeType> changeTypes = ResponseHelper.getChangeTypes(request, l3Response,
        "https://static-uatb.gdn-app.com");

    assertEquals(8, changeTypes.size(), "Should detect all 10 possible changes");
    Assertions.assertTrue(changeTypes.contains(L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE), "Should detect product name change");
    Assertions.assertTrue(changeTypes.contains(L3InfoUpdateChangeType.INSTORE_UPDATE), "Should detect instore status change");
    Assertions.assertTrue(changeTypes.contains(L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE), "Should detect product type change");
    Assertions.assertTrue(changeTypes.contains(L3InfoUpdateChangeType.DIMENSIONS_UPDATE), "Should detect dimensions change");
    Assertions.assertTrue(changeTypes.contains(L3InfoUpdateChangeType.SIZE_CHART_UPDATE), "Should detect size chart change");
    Assertions.assertTrue(changeTypes.contains(L3InfoUpdateChangeType.YOUTUBE_URL_UPDATE), "Should detect YouTube URL change");
  }

  @Test
  void testGetChangeTypes_NoChange_ShouldReturnEmptySet() {
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    masterDataProduct.setProductName("Same Product");
    masterDataProduct.setDescription("Same Description");

    ProductL3Response l3Response = new ProductL3Response();
    l3Response.setMasterDataProduct(masterDataProduct);
    l3Response.setOff2OnChannelActive(true);
    l3Response.setProductType(ProductType.REGULAR);
    l3Response.setLength(2.0);
    l3Response.setWidth(2.0);
    l3Response.setHeight(2.0);
    l3Response.setWeight(2.0);
    l3Response.setShippingWeight(2.0);
    l3Response.setSizeChartCode("sameSize");
    l3Response.setVideoUrl("https://same.video");
    l3Response.setUrl("https://same.youtube");

    BulkBasicInfoUpdateRequest request = BulkBasicInfoUpdateRequest.builder()
        .productName("Same Product")
        .inStore(true)
        .description("Same Description")
        .shippingType("Dikirim oleh seller")
        .length(2.0)
        .width(2.0)
        .height(2.0)
        .weight(2.0)
        .shippingWeight(2.0)
        .sizeChartCode("sameSize")
        .videoUrl("https://same.youtube")
        .youtubeUrl(true)
        .imagesUpdated(false)
        .build();

    Set<L3InfoUpdateChangeType> changeTypes = ResponseHelper.getChangeTypes(request, l3Response,
        "https://static-uatb.gdn-app.com");
  }

  @Test
  void testGetChangeTypes_VideoOnlyUpdate() {
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();

    ProductL3Response l3Response = new ProductL3Response();
    l3Response.setMasterDataProduct(masterDataProduct);
    l3Response.setOff2OnChannelActive(true);
    l3Response.setProductType(ProductType.BOPIS);
    l3Response.setLength(1.0);
    l3Response.setWidth(1.0);
    l3Response.setHeight(1.0);
    l3Response.setWeight(1.0);
    l3Response.setShippingWeight(1.0);
    l3Response.setSizeChartCode("A");
    l3Response.setUrl("https://youtube.com/old");
    l3Response.setVideoUrl("https://video.com/old");

    BulkBasicInfoUpdateRequest request = BulkBasicInfoUpdateRequest.builder()
        .productName(null)
        .inStore(true)
        .description(null)
        .shippingType("Dikirim oleh blibli")
        .length(1.0)
        .width(1.0)
        .height(1.0)
        .weight(1.0)
        .shippingWeight(1.0)
        .sizeChartCode("A")
        .videoUrl("https://video.com/new")  // different from l3Response
        .youtubeUrl(false)
        .imagesUpdated(false)
        .build();

    Set<L3InfoUpdateChangeType> changeTypes = ResponseHelper.getChangeTypes(request, l3Response,
        "https://static-uatb.gdn-app.com");

    assertEquals(3, changeTypes.size(), "Should detect exactly 3 changes");
    Assertions.assertTrue(changeTypes.contains(L3InfoUpdateChangeType.VIDEO_UPDATE), "Should detect video URL change");
  }

  @Test
  void testGetChangeTypes_LengthChanged_ShouldReturnDimensionsUpdate() {
    BulkBasicInfoUpdateRequest request = BulkBasicInfoUpdateRequest.builder()
        .productName("Same Product")
        .inStore(true)
        .description("Same Description")
        .shippingType("Dikirim oleh blibli")
        .length(2.0)  // Changed from 1.0
        .width(1.0)
        .height(1.0)
        .weight(1.0)
        .shippingWeight(1.0)
        .sizeChartCode("A")
        .videoUrl(null)
        .youtubeUrl(false)
        .imagesUpdated(false)
        .build();

    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    masterDataProduct.setProductName("Same Product");
    masterDataProduct.setDescription("Same Description");

    ProductL3Response l3Response = new ProductL3Response();
    l3Response.setMasterDataProduct(masterDataProduct);
    l3Response.setOff2OnChannelActive(true);
    l3Response.setProductType(ProductType.REGULAR);
    l3Response.setLength(1.0);
    l3Response.setWidth(1.0);
    l3Response.setHeight(1.0);
    l3Response.setWeight(1.0);
    l3Response.setShippingWeight(1.0);
    l3Response.setSizeChartCode("A");
    l3Response.setUrl(null);
    l3Response.setVideoUrl(null);

    Set<L3InfoUpdateChangeType> changeTypes = ResponseHelper.getChangeTypes(request, l3Response,
        "https://static-uatb.gdn-app.com");

    assertEquals(1, changeTypes.size(), "Should detect exactly 2 changes");
    Assertions.assertTrue(changeTypes.contains(L3InfoUpdateChangeType.DIMENSIONS_UPDATE), "Should detect dimensions change");
  }

  @Test
  void testGetChangeTypes_WidthChanged_ShouldReturnDimensionsUpdate() {
    BulkBasicInfoUpdateRequest request = BulkBasicInfoUpdateRequest.builder()
        .productName("Same Product")
        .inStore(true)
        .description("Same Description")
        .shippingType("Dikirim oleh blibli")
        .length(1.0)
        .width(2.0)  // Changed from 1.0
        .height(1.0)
        .weight(1.0)
        .shippingWeight(1.0)
        .sizeChartCode("A")
        .videoUrl(null)
        .youtubeUrl(false)
        .imagesUpdated(false)
        .build();

    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    masterDataProduct.setProductName("Same Product");
    masterDataProduct.setDescription("Same Description");

    ProductL3Response l3Response = new ProductL3Response();
    l3Response.setMasterDataProduct(masterDataProduct);
    l3Response.setOff2OnChannelActive(true);
    l3Response.setProductType(ProductType.REGULAR);
    l3Response.setLength(1.0);
    l3Response.setWidth(1.0);
    l3Response.setHeight(1.0);
    l3Response.setWeight(1.0);
    l3Response.setShippingWeight(1.0);
    l3Response.setSizeChartCode("A");
    l3Response.setUrl(null);
    l3Response.setVideoUrl(null);

    Set<L3InfoUpdateChangeType> changeTypes = ResponseHelper.getChangeTypes(request, l3Response,
        "https://static-uatb.gdn-app.com");

    assertEquals(1, changeTypes.size(), "Should detect exactly 2 changes");
    Assertions.assertTrue(changeTypes.contains(L3InfoUpdateChangeType.DIMENSIONS_UPDATE), "Should detect dimensions change");
  }

  @Test
  void testGetChangeTypes_HeightChanged_ShouldReturnDimensionsUpdate() {
    BulkBasicInfoUpdateRequest request = BulkBasicInfoUpdateRequest.builder()
        .productName("Same Product")
        .inStore(true)
        .description("Same Description")
        .shippingType("Dikirim oleh blibli")
        .length(1.0)
        .width(1.0)
        .height(2.0)  // Changed from 1.0
        .weight(1.0)
        .shippingWeight(1.0)
        .sizeChartCode("A")
        .videoUrl(null)
        .youtubeUrl(false)
        .imagesUpdated(false)
        .build();

    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    masterDataProduct.setProductName("Same Product");
    masterDataProduct.setDescription("Same Description");

    ProductL3Response l3Response = new ProductL3Response();
    l3Response.setMasterDataProduct(masterDataProduct);
    l3Response.setOff2OnChannelActive(true);
    l3Response.setProductType(ProductType.REGULAR);
    l3Response.setLength(1.0);
    l3Response.setWidth(1.0);
    l3Response.setHeight(1.0);
    l3Response.setWeight(1.0);
    l3Response.setShippingWeight(1.0);
    l3Response.setSizeChartCode("A");
    l3Response.setUrl(null);
    l3Response.setVideoUrl(null);

    Set<L3InfoUpdateChangeType> changeTypes = ResponseHelper.getChangeTypes(request, l3Response,
        "https://static-uatb.gdn-app.com");

    assertEquals(1, changeTypes.size(), "Should detect exactly 2 changes");
    Assertions.assertTrue(changeTypes.contains(L3InfoUpdateChangeType.DIMENSIONS_UPDATE), "Should detect dimensions change");
  }

  @Test
  void testGetChangeTypes_WeightChanged_ShouldReturnDimensionsUpdate() {
    BulkBasicInfoUpdateRequest request = BulkBasicInfoUpdateRequest.builder()
        .productName("Same Product")
        .inStore(true)
        .description("Same Description")
        .shippingType("Dikirim oleh bliblii")
        .length(1.0)
        .width(1.0)
        .height(1.0)
        .weight(2.0)  // Changed from 1.0
        .shippingWeight(1.0)
        .sizeChartCode("A")
        .videoUrl(null)
        .youtubeUrl(false)
        .imagesUpdated(false)
        .build();

    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    masterDataProduct.setProductName("Same Product");
    masterDataProduct.setDescription("Same Description");

    ProductL3Response l3Response = new ProductL3Response();
    l3Response.setMasterDataProduct(masterDataProduct);
    l3Response.setOff2OnChannelActive(true);
    l3Response.setProductType(ProductType.REGULAR);
    l3Response.setLength(1.0);
    l3Response.setWidth(1.0);
    l3Response.setHeight(1.0);
    l3Response.setWeight(1.0);
    l3Response.setShippingWeight(1.0);
    l3Response.setSizeChartCode("A");
    l3Response.setUrl(null);
    l3Response.setVideoUrl(null);

    Set<L3InfoUpdateChangeType> changeTypes = ResponseHelper.getChangeTypes(request, l3Response,
        "https://static-uatb.gdn-app.com");

    assertEquals(2, changeTypes.size(), "Should detect exactly 2 changes");
    Assertions.assertTrue(changeTypes.contains(L3InfoUpdateChangeType.DIMENSIONS_UPDATE), "Should detect dimensions change");
  }

  @Test
  void testGetChangeTypes_ShippingWeightChanged_ShouldReturnDimensionsUpdate() {
    BulkBasicInfoUpdateRequest request = BulkBasicInfoUpdateRequest.builder()
        .productName("Same Product")
        .inStore(true)
        .description("Same Description")
        .shippingType("Dikirim oleh blibli")
        .length(1.0)
        .width(1.0)
        .height(1.0)
        .weight(1.0)
        .shippingWeight(2.0)  // Changed from 1.0
        .sizeChartCode("A")
        .videoUrl("ddd")
        .youtubeUrl(false)
        .imagesUpdated(false)
        .build();

    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    masterDataProduct.setProductName("Same Product");
    masterDataProduct.setDescription("Same Description");

    ProductL3Response l3Response = new ProductL3Response();
    l3Response.setMasterDataProduct(masterDataProduct);
    l3Response.setOff2OnChannelActive(true);
    l3Response.setProductType(ProductType.REGULAR);
    l3Response.setLength(1.0);
    l3Response.setWidth(1.0);
    l3Response.setHeight(1.0);
    l3Response.setWeight(1.0);
    l3Response.setShippingWeight(1.0);
    l3Response.setSizeChartCode("A");
    l3Response.setUrl(null);
    l3Response.setVideoUrl("ddd");

    Set<L3InfoUpdateChangeType> changeTypes = ResponseHelper.getChangeTypes(request, l3Response,
        "https://static-uatb.gdn-app.com");

    assertEquals(1, changeTypes.size(), "Should detect exactly 2 changes");
    Assertions.assertTrue(changeTypes.contains(L3InfoUpdateChangeType.DIMENSIONS_UPDATE), "Should detect dimensions change");
  }

  @Test
  void testGetChangeTypes_ShippingWeightChanged_ShouldReturnDimensionsUpdateEmptyVideo() {
    BulkBasicInfoUpdateRequest request =
        BulkBasicInfoUpdateRequest.builder().productName("Same Product").inStore(true)
            .description("Same Description").shippingType("Dikirim oleh blibli").length(1.0)
            .width(1.0).height(1.0).weight(1.0).shippingWeight(2.0)  // Changed from 1.0
            .sizeChartCode("A").youtubeUrl(false).imagesUpdated(false).build();
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    masterDataProduct.setProductName("Same Product");
    masterDataProduct.setDescription("Same Description");
    ProductL3Response l3Response = new ProductL3Response();
    l3Response.setMasterDataProduct(masterDataProduct);
    l3Response.setOff2OnChannelActive(true);
    l3Response.setProductType(ProductType.REGULAR);
    l3Response.setLength(1.0);
    l3Response.setWidth(1.0);
    l3Response.setHeight(1.0);
    l3Response.setWeight(1.0);
    l3Response.setShippingWeight(1.0);
    l3Response.setSizeChartCode("A");
    l3Response.setUrl(null);
    l3Response.setVideoUrl("ddd");
    Set<L3InfoUpdateChangeType> changeTypes =
        ResponseHelper.getChangeTypes(request, l3Response, "https://static-uatb.gdn-app.com");
    assertEquals(2, changeTypes.size(), "Should detect exactly 2 changes");
    Assertions.assertTrue(changeTypes.contains(L3InfoUpdateChangeType.DIMENSIONS_UPDATE),
        "Should detect dimensions change");
  }

  @Test
  void testGetChangeTypes_ShippingWeightChanged_ShouldReturnDimensionsUpdateEmptyYoutbe() {
    BulkBasicInfoUpdateRequest request =
        BulkBasicInfoUpdateRequest.builder().productName("Same Product").inStore(true)
            .description("Same Description").shippingType("Dikirim oleh blibli").length(1.0)
            .width(1.0).height(1.0).weight(1.0).shippingWeight(2.0)  // Changed from 1.0
            .sizeChartCode("A").youtubeUrl(false).imagesUpdated(false).build();
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    masterDataProduct.setProductName("Same Product");
    masterDataProduct.setDescription("Same Description");
    ProductL3Response l3Response = new ProductL3Response();
    l3Response.setMasterDataProduct(masterDataProduct);
    l3Response.setOff2OnChannelActive(true);
    l3Response.setProductType(ProductType.REGULAR);
    l3Response.setLength(1.0);
    l3Response.setWidth(1.0);
    l3Response.setHeight(1.0);
    l3Response.setWeight(1.0);
    l3Response.setShippingWeight(1.0);
    l3Response.setSizeChartCode("A");
    l3Response.setUrl("dddd");
    Set<L3InfoUpdateChangeType> changeTypes =
        ResponseHelper.getChangeTypes(request, l3Response, "https://static-uatb.gdn-app.com");
    assertEquals(2, changeTypes.size(), "Should detect exactly 2 changes");
    Assertions.assertTrue(changeTypes.contains(L3InfoUpdateChangeType.DIMENSIONS_UPDATE),
        "Should detect dimensions change");
  }

  @Test
  void testGetChangeTypes_AllDimensionsChanged_ShouldReturnDimensionsUpdate() {
    BulkBasicInfoUpdateRequest request = BulkBasicInfoUpdateRequest.builder()
        .productName("Same Product")
        .inStore(true)
        .description("Same Description")
        .shippingType("Dikirim oleh blibli")
        .length(2.0)  // Changed
        .width(2.0)   // Changed
        .height(2.0)  // Changed
        .weight(2.0)  // Changed
        .shippingWeight(2.0)  // Changed
        .sizeChartCode("A")
        .videoUrl(null)
        .youtubeUrl(false)
        .imagesUpdated(false)
        .build();

    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    masterDataProduct.setProductName("Same Product");
    masterDataProduct.setDescription("Same Description");

    ProductL3Response l3Response = new ProductL3Response();
    l3Response.setMasterDataProduct(masterDataProduct);
    l3Response.setOff2OnChannelActive(true);
    l3Response.setProductType(ProductType.REGULAR);
    l3Response.setLength(1.0);
    l3Response.setWidth(1.0);
    l3Response.setHeight(1.0);
    l3Response.setWeight(1.0);
    l3Response.setShippingWeight(1.0);
    l3Response.setSizeChartCode("A");
    l3Response.setUrl(null);
    l3Response.setVideoUrl(null);

    Set<L3InfoUpdateChangeType> changeTypes = ResponseHelper.getChangeTypes(request, l3Response,
        "https://static-uatb.gdn-app.com");

    assertEquals(1, changeTypes.size(), "Should detect exactly 2 changes");
    Assertions.assertTrue(changeTypes.contains(L3InfoUpdateChangeType.DIMENSIONS_UPDATE), "Should detect dimensions change");
  }

  @Test
  void shouldMarkNewImageWithReviewTypeNew() {
    BulkBasicInfoUpdateRequest request = new BulkBasicInfoUpdateRequest();
    request.setCommonImages(List.of("url1"));
    request.setMainImageUrl("url1");

    BulkProcessImage bulkImage = new BulkProcessImage();
    bulkImage.setLocation("loc1");
    bulkImage.setSequence(1);

    Map<String, BulkProcessImage> imageMap = Map.of("url1", bulkImage);

    ProductL3Response response = new ProductL3Response();
    MasterDataProductDTO masterData = new MasterDataProductDTO();
    masterData.setMasterDataProductImages(Collections.emptyList());
    response.setMasterDataProduct(masterData);

    List<ProductLevel3SummaryDetailsImageRequest> result = ResponseHelper.generateCommonImageRequestForEdit(response, request, imageMap,
        "", "");

    assertEquals(1, result.size());
    ProductLevel3SummaryDetailsImageRequest img = result.get(0);
    assertEquals("new", img.getReviewType());
    Assertions.assertTrue(request.isImagesUpdated());
  }

  @Test
  void shouldRetainExistingImageWithoutChange() {
    BulkBasicInfoUpdateRequest request = new BulkBasicInfoUpdateRequest();
    request.setCommonImages(List.of("url1"));
    request.setMainImageUrl("url1");

    BulkProcessImage bulkImage = new BulkProcessImage();
    bulkImage.setLocation("loc1");
    bulkImage.setSequence(1);

    Map<String, BulkProcessImage> imageMap = Map.of("url1", bulkImage);

    MasterDataProductImageDTO existingImage = new MasterDataProductImageDTO();
    existingImage.setLocationPath("loc1");
    existingImage.setMainImage(true);
    existingImage.setSequence(1);

    ProductL3Response response = new ProductL3Response();
    MasterDataProductDTO masterData = new MasterDataProductDTO();
    masterData.setMasterDataProductImages(List.of(existingImage));
    response.setMasterDataProduct(masterData);

    List<ProductLevel3SummaryDetailsImageRequest> result = ResponseHelper.generateCommonImageRequestForEdit(response, request, imageMap,
        "imageStaticBaseUrlPrefix", "");

    assertEquals(1, result.size());
    ProductLevel3SummaryDetailsImageRequest img = result.get(0);
    Assertions.assertNotNull(img.getReviewType());
    Assertions.assertFalse(img.getMarkForDelete());
    Assertions.assertTrue(request.isImagesUpdated());
  }

  @Test
  void shouldMarkUpdateWhenMainImageChanged() {
    BulkBasicInfoUpdateRequest request = new BulkBasicInfoUpdateRequest();
    request.setCommonImages(List.of("url1"));
    request.setMainImageUrl("url1");

    BulkProcessImage bulkImage = new BulkProcessImage();
    bulkImage.setLocation("loc1");
    bulkImage.setSequence(1);

    Map<String, BulkProcessImage> imageMap = Map.of("url1", bulkImage);

    MasterDataProductImageDTO existingImage = new MasterDataProductImageDTO();
    existingImage.setLocationPath("loc1");
    existingImage.setMainImage(false); // previously not main
    existingImage.setSequence(1);

    ProductL3Response response = new ProductL3Response();
    MasterDataProductDTO masterData = new MasterDataProductDTO();
    masterData.setMasterDataProductImages(List.of(existingImage));
    response.setMasterDataProduct(masterData);

    List<ProductLevel3SummaryDetailsImageRequest> result = ResponseHelper.generateCommonImageRequestForEdit(response, request, imageMap,
        "imageStaticBaseUrlPrefix", "");

    assertEquals(1, result.size());
    ProductLevel3SummaryDetailsImageRequest img = result.get(0);
    assertEquals("new", img.getReviewType());
    Assertions.assertTrue(request.isImagesUpdated());
  }

  @Test
  void shouldMarkDeletedImage() {
    BulkBasicInfoUpdateRequest request = new BulkBasicInfoUpdateRequest();
    request.setCommonImages(Collections.emptyList());
    request.setMainImageUrl("url1");

    Map<String, BulkProcessImage> imageMap = Collections.emptyMap();

    MasterDataProductImageDTO existingImage = new MasterDataProductImageDTO();
    existingImage.setLocationPath("loc1");
    existingImage.setMainImage(false);
    existingImage.setSequence(1);

    ProductL3Response response = new ProductL3Response();
    MasterDataProductDTO masterData = new MasterDataProductDTO();
    masterData.setMasterDataProductImages(List.of(existingImage));
    response.setMasterDataProduct(masterData);

    List<ProductLevel3SummaryDetailsImageRequest> result = ResponseHelper.generateCommonImageRequestForEdit(response, request, imageMap,
        "imageStaticBaseUrlPrefix", "");

    assertEquals(1, result.size());
    ProductLevel3SummaryDetailsImageRequest img = result.get(0);
    Assertions.assertTrue(img.getMarkForDelete());
    assertEquals("update", img.getReviewType());
    Assertions.assertTrue(request.isImagesUpdated());
  }

  @Test
  void shouldHandleNoImagesGracefully() {
    BulkBasicInfoUpdateRequest request = new BulkBasicInfoUpdateRequest();
    request.setCommonImages(null);
    request.setMainImageUrl("mainImageUrl");

    ProductL3Response response = new ProductL3Response();
    MasterDataProductDTO masterData = new MasterDataProductDTO();
    masterData.setMasterDataProductImages(null);
    response.setMasterDataProduct(masterData);

    List<ProductLevel3SummaryDetailsImageRequest> result = ResponseHelper.generateCommonImageRequestForEdit(response, request, Map.of(),
        "imageStaticBaseUrlPrefix", "");

    Assertions.assertTrue(result.isEmpty());
    Assertions.assertFalse(request.isImagesUpdated());
  }

  @Test
  void shouldBulkImageEmptyTest() {
    BulkBasicInfoUpdateRequest request = new BulkBasicInfoUpdateRequest();
    request.setCommonImages(List.of("url1"));
    request.setMainImageUrl("url1");

    BulkProcessImage bulkImage = new BulkProcessImage();
    bulkImage.setLocation("loc11");
    bulkImage.setSequence(1);

    Map<String, BulkProcessImage> imageMap = Map.of("url12312", bulkImage);

    ProductL3Response response = new ProductL3Response();
    MasterDataProductDTO masterData = new MasterDataProductDTO();
    masterData.setMasterDataProductImages(Collections.emptyList());
    response.setMasterDataProduct(masterData);

    List<ProductLevel3SummaryDetailsImageRequest> result = ResponseHelper.generateCommonImageRequestForEdit(response, request, imageMap,
        "imageStaticBaseUrlPrefix", "");

    assertEquals(1, result.size());
  }

  @Test
  void shouldUnsetMainImageWhenRetainedButNotMain() {
    // Test the condition: StringUtils.equals(existingMainImage, prefixRemovedImageUrl) && isExistingMainImageChanged
    BulkBasicInfoUpdateRequest request = new BulkBasicInfoUpdateRequest();
    request.setCommonImages(List.of("currentMain", "newMain"));
    request.setMainImageUrl("newMain"); // Change main image from currentMain to newMain

    // currentMain is existing image (no BulkProcessImage)
    // newMain is new image (has BulkProcessImage)
    BulkProcessImage newMainBulkImage = new BulkProcessImage();
    newMainBulkImage.setLocation("newMainLoc");
    newMainBulkImage.setSequence(1);

    Map<String, BulkProcessImage> imageMap = Map.of("newMain", newMainBulkImage);

    // Setup existing main image that will be retained but lose main status
    MasterDataProductImageDTO currentMainImage = new MasterDataProductImageDTO();
    currentMainImage.setLocationPath("currentMain");
    currentMainImage.setMainImage(true); // Currently main
    currentMainImage.setSequence(0);

    ProductL3Response response = new ProductL3Response();
    MasterDataProductDTO masterData = new MasterDataProductDTO();
    masterData.setMasterDataProductImages(List.of(currentMainImage));
    response.setMasterDataProduct(masterData);

    List<ProductLevel3SummaryDetailsImageRequest> result = ResponseHelper.generateCommonImageRequestForEdit(response, request, imageMap,
        "imageStaticBaseUrlPrefix", "");

    // Should create 2 requests:
    // 1. Unset main for currentMain (the condition we're testing)
    // 2. Set main for newMain
    assertEquals(2, result.size());
    
    // Find request to unset main for currentMain - this tests our target condition
    ProductLevel3SummaryDetailsImageRequest unsetMainRequest = result.stream()
        .filter(req -> "currentMain".equals(req.getLocationPath()) && !req.getMainImage())
        .findFirst().orElse(null);
    Assertions.assertNotNull(unsetMainRequest);
    assertEquals("currentMain", unsetMainRequest.getLocationPath());
    Assertions.assertFalse(unsetMainRequest.getMainImage());
    assertEquals("update", unsetMainRequest.getReviewType());
    
    // Find request to set main for newMain
    ProductLevel3SummaryDetailsImageRequest setMainRequest = result.stream()
        .filter(req -> "newMainLoc".equals(req.getLocationPath()) && req.getMainImage())
        .findFirst().orElse(null);
    Assertions.assertTrue(request.isImagesUpdated());
  }

  @Test
  void shouldSetMainImageForExistingImage() {
    // Test the condition: isExistingMainImageChanged && StringUtils.equals(uploadedMainImageUrl, prefixRemovedImageUrl)
    BulkBasicInfoUpdateRequest request = new BulkBasicInfoUpdateRequest();
    request.setCommonImages(List.of("newMain"));
    request.setMainImageUrl("newMain"); // This existing image should become main

    // No BulkProcessImage - this is an existing image
    Map<String, BulkProcessImage> imageMap = Map.of();

    // Setup existing images - one currently main, one that should become main
    MasterDataProductImageDTO currentMainImage = new MasterDataProductImageDTO();
    currentMainImage.setLocationPath("oldMain");
    currentMainImage.setMainImage(true);
    currentMainImage.setSequence(0);

    MasterDataProductImageDTO newMainImage = new MasterDataProductImageDTO();
    newMainImage.setLocationPath("newMain");
    newMainImage.setMainImage(false);
    newMainImage.setSequence(1);

    ProductL3Response response = new ProductL3Response();
    MasterDataProductDTO masterData = new MasterDataProductDTO();
    masterData.setMasterDataProductImages(List.of(currentMainImage, newMainImage));
    response.setMasterDataProduct(masterData);

    List<ProductLevel3SummaryDetailsImageRequest> result = ResponseHelper.generateCommonImageRequestForEdit(response, request, imageMap,
        "imageStaticBaseUrlPrefix", "");

    // Should create 1 request - the else if condition creates a request with existingMainImage path but sets as main
    assertEquals(2, result.size());

    ProductLevel3SummaryDetailsImageRequest imageRequest = result.get(0);
    Assertions.assertTrue(imageRequest.getMainImage());
    assertEquals("update", imageRequest.getReviewType());
    assertEquals(Integer.valueOf(0), imageRequest.getSequence());

    Assertions.assertTrue(request.isImagesUpdated());
  }

  @Test
  void shouldHandleCondition_TrueAndFalse() {
    // Test: StringUtils.equals(existingMainImage, prefixRemovedImageUrl) = TRUE && isExistingMainImageChanged = FALSE
    // Scenario: Current main image is in upload list AND main image is NOT changing (same main remains main)
    BulkBasicInfoUpdateRequest request = new BulkBasicInfoUpdateRequest();
    request.setCommonImages(List.of("mainImage"));
    request.setMainImageUrl("mainImage"); // Same main image as before

    // No BulkProcessImage - existing image
    Map<String, BulkProcessImage> imageMap = Map.of();

    // Setup: current main image that stays main
    MasterDataProductImageDTO existingMainImage = new MasterDataProductImageDTO();
    existingMainImage.setLocationPath("mainImage");
    existingMainImage.setMainImage(true); // Currently main
    existingMainImage.setSequence(0);

    ProductL3Response response = new ProductL3Response();
    MasterDataProductDTO masterData = new MasterDataProductDTO();
    masterData.setMasterDataProductImages(List.of(existingMainImage));
    response.setMasterDataProduct(masterData);

    List<ProductLevel3SummaryDetailsImageRequest> result = ResponseHelper.generateCommonImageRequestForEdit(response, request, imageMap,
        "imageStaticBaseUrlPrefix", "");
    assertEquals(0, result.size());
    Assertions.assertFalse(request.isImagesUpdated());
  }

  @Test
  void shouldHandleCondition_FalseAndTrue() {
    // Test: StringUtils.equals(existingMainImage, prefixRemovedImageUrl) = FALSE && isExistingMainImageChanged = TRUE
    // Scenario: Processing a non-main image while main image is changing to something else
    BulkBasicInfoUpdateRequest request = new BulkBasicInfoUpdateRequest();
    request.setCommonImages(List.of("nonMainImage", "newMainImage"));
    request.setMainImageUrl("newMainImage"); // Main changes from oldMain to newMainImage

    // Both are existing images (no BulkProcessImage)
    Map<String, BulkProcessImage> imageMap = Map.of();

    // Setup: old main image and other images
    MasterDataProductImageDTO oldMainImage = new MasterDataProductImageDTO();
    oldMainImage.setLocationPath("oldMain");
    oldMainImage.setMainImage(true); // Currently main
    oldMainImage.setSequence(0);

    MasterDataProductImageDTO nonMainImage = new MasterDataProductImageDTO();
    nonMainImage.setLocationPath("nonMainImage");
    nonMainImage.setMainImage(false);
    nonMainImage.setSequence(1);

    MasterDataProductImageDTO newMainImage = new MasterDataProductImageDTO();
    newMainImage.setLocationPath("newMainImage");
    newMainImage.setMainImage(false);
    newMainImage.setSequence(2);

    ProductL3Response response = new ProductL3Response();
    MasterDataProductDTO masterData = new MasterDataProductDTO();
    masterData.setMasterDataProductImages(List.of(oldMainImage, nonMainImage, newMainImage));
    response.setMasterDataProduct(masterData);

    List<ProductLevel3SummaryDetailsImageRequest> result = ResponseHelper.generateCommonImageRequestForEdit(response, request, imageMap,
        "imageStaticBaseUrlPrefix", "");
    assertEquals(2, result.size());
    ProductLevel3SummaryDetailsImageRequest img = result.get(0);
    Assertions.assertTrue(img.getMainImage());
    assertEquals("update", img.getReviewType());
    Assertions.assertTrue(request.isImagesUpdated());
  }

  @Test
  void shouldHandleCondition_FalseAndFalse() {
    BulkBasicInfoUpdateRequest request = new BulkBasicInfoUpdateRequest();
    request.setCommonImages(List.of("nonMainImage"));
    request.setMainImageUrl("currentMain"); // Same main image as before (no change)
    Map<String, BulkProcessImage> imageMap = Map.of();
    MasterDataProductImageDTO currentMainImage = new MasterDataProductImageDTO();
    currentMainImage.setLocationPath("currentMain");
    currentMainImage.setMainImage(true); // Currently main
    currentMainImage.setSequence(0);

    MasterDataProductImageDTO nonMainImage = new MasterDataProductImageDTO();
    nonMainImage.setLocationPath("nonMainImage");
    nonMainImage.setMainImage(false);
    nonMainImage.setSequence(1);

    ProductL3Response response = new ProductL3Response();
    MasterDataProductDTO masterData = new MasterDataProductDTO();
    masterData.setMasterDataProductImages(List.of(currentMainImage, nonMainImage));
    response.setMasterDataProduct(masterData);

    List<ProductLevel3SummaryDetailsImageRequest> result = ResponseHelper.generateCommonImageRequestForEdit(response, request, imageMap,
        "imageStaticBaseUrlPrefix", "");
    assertEquals(1, result.size());
  }

  @Test
  void shouldHandlePrefixRemoval_NewImage() {
    String prefix = "https://www.static.blibli.com/";
    BulkBasicInfoUpdateRequest request = new BulkBasicInfoUpdateRequest();
    request.setCommonImages(List.of(prefix + "image1.jpg", prefix + "image2.jpg"));
    request.setMainImageUrl(prefix + "image1.jpg"); // Main image with prefix

    BulkProcessImage bulkImage1 = new BulkProcessImage();
    bulkImage1.setLocation("processed/image1.jpg");
    bulkImage1.setSequence(0);

    BulkProcessImage bulkImage2 = new BulkProcessImage();
    bulkImage2.setLocation("processed/image2.jpg");
    bulkImage2.setSequence(1);
    Map<String, BulkProcessImage> imageMap = Map.of(
        prefix + "image1.jpg", bulkImage1,
        prefix + "image2.jpg", bulkImage2
    );
    ProductL3Response response = new ProductL3Response();
    MasterDataProductDTO masterData = new MasterDataProductDTO();
    masterData.setMasterDataProductImages(Collections.emptyList());
    response.setMasterDataProduct(masterData);
    List<ProductLevel3SummaryDetailsImageRequest> result = ResponseHelper.generateCommonImageRequestForEdit(response, request, imageMap, prefix,
        "");
    assertEquals(2, result.size());
    ProductLevel3SummaryDetailsImageRequest mainImg = result.stream()
        .filter(img -> img.getMainImage())
        .findFirst().orElse(null);
    Assertions.assertNotNull(mainImg);
    Assertions.assertTrue(mainImg.getMainImage());
    assertEquals("new", mainImg.getReviewType());
    ProductLevel3SummaryDetailsImageRequest nonMainImg = result.stream()
        .filter(img -> !img.getMainImage())
        .findFirst().orElse(null);
    Assertions.assertNotNull(nonMainImg);
    Assertions.assertFalse(nonMainImg.getMainImage());
    assertEquals("new", nonMainImg.getReviewType());
    Assertions.assertTrue(request.isImagesUpdated());
  }

  @Test
  void shouldHandlePrefixRemoval_ExistingImage() {
    String prefix = "https://static.example.com/";
    BulkBasicInfoUpdateRequest request = new BulkBasicInfoUpdateRequest();
    request.setCommonImages(List.of(prefix + "existing.jpg"));
    request.setMainImageUrl(prefix + "existing.jpg");
    Map<String, BulkProcessImage> imageMap = Map.of();
    MasterDataProductImageDTO existingImage = new MasterDataProductImageDTO();
    existingImage.setLocationPath("existing.jpg");
    existingImage.setMainImage(true);
    existingImage.setSequence(0);

    ProductL3Response response = new ProductL3Response();
    MasterDataProductDTO masterData = new MasterDataProductDTO();
    masterData.setMasterDataProductImages(List.of(existingImage));
    response.setMasterDataProduct(masterData);

    List<ProductLevel3SummaryDetailsImageRequest> result = ResponseHelper.generateCommonImageRequestForEdit(response, request, imageMap, prefix,
        "");
    assertEquals(0, result.size());
    Assertions.assertFalse(request.isImagesUpdated());
  }

  @Test
  void shouldHandlePrefixRemoval_MainImageChange() {
    String prefix = "https://cdn.example.com/images/";
    BulkBasicInfoUpdateRequest request = new BulkBasicInfoUpdateRequest();
    request.setCommonImages(List.of(prefix + "oldMain.jpg", prefix + "newMain.jpg"));
    request.setMainImageUrl(prefix + "newMain.jpg");
    Map<String, BulkProcessImage> imageMap = Map.of();

    MasterDataProductImageDTO oldMainImage = new MasterDataProductImageDTO();
    oldMainImage.setLocationPath("oldMain.jpg");
    oldMainImage.setMainImage(true);
    oldMainImage.setSequence(0);

    MasterDataProductImageDTO newMainImage = new MasterDataProductImageDTO();
    newMainImage.setLocationPath("newMain.jpg");
    newMainImage.setMainImage(false);
    newMainImage.setSequence(1);

    ProductL3Response response = new ProductL3Response();
    MasterDataProductDTO masterData = new MasterDataProductDTO();
    masterData.setMasterDataProductImages(List.of(oldMainImage, newMainImage));
    response.setMasterDataProduct(masterData);

    List<ProductLevel3SummaryDetailsImageRequest> result = ResponseHelper.generateCommonImageRequestForEdit(response, request, imageMap, prefix,
        "");

    assertEquals(2, result.size());
    ProductLevel3SummaryDetailsImageRequest unsetMainRequest = result.stream()
        .filter(req -> "oldMain.jpg".equals(req.getLocationPath()) && !req.getMainImage())
        .findFirst().orElse(null);
    Assertions.assertNotNull(unsetMainRequest);
    assertEquals("update", unsetMainRequest.getReviewType());
    ProductLevel3SummaryDetailsImageRequest setMainRequest = result.stream()
        .filter(req -> req.getMainImage())
        .findFirst().orElse(null);
    Assertions.assertNotNull(setMainRequest);
    assertEquals("update", setMainRequest.getReviewType());

    Assertions.assertTrue(request.isImagesUpdated());
  }

  @Test
  void shouldHandleMixedPrefixAndNonPrefix() {
    String prefix = "https://static.test.com/";
    BulkBasicInfoUpdateRequest request = new BulkBasicInfoUpdateRequest();
    request.setCommonImages(List.of(prefix + "withPrefix.jpg", "withoutPrefix.jpg"));
    request.setMainImageUrl("withoutPrefix.jpg");

    BulkProcessImage bulkImage1 = new BulkProcessImage();
    bulkImage1.setLocation("processed/withPrefix.jpg");
    bulkImage1.setSequence(0);

    BulkProcessImage bulkImage2 = new BulkProcessImage();
    bulkImage2.setLocation("processed/withoutPrefix.jpg");
    bulkImage2.setSequence(1);

    Map<String, BulkProcessImage> imageMap = Map.of(
        prefix + "withPrefix.jpg", bulkImage1,
        "withoutPrefix.jpg", bulkImage2
    );

    ProductL3Response response = new ProductL3Response();
    MasterDataProductDTO masterData = new MasterDataProductDTO();
    masterData.setMasterDataProductImages(Collections.emptyList());
    response.setMasterDataProduct(masterData);

    List<ProductLevel3SummaryDetailsImageRequest> result = ResponseHelper.generateCommonImageRequestForEdit(response, request, imageMap, prefix,
        "");

    assertEquals(2, result.size());
    ProductLevel3SummaryDetailsImageRequest prefixImg = result.stream()
        .filter(req -> "processed/withPrefix.jpg".equals(req.getLocationPath()))
        .findFirst().orElse(null);
    Assertions.assertTrue(request.isImagesUpdated());
  }

  @Test
  void shouldHandleMainImageUrlPrefixRemoval() {
    String prefix = "https://images.example.com/";
    BulkBasicInfoUpdateRequest request = new BulkBasicInfoUpdateRequest();
    request.setCommonImages(List.of("image1.jpg"));
    request.setMainImageUrl(prefix + "image1.jpg");

    BulkProcessImage bulkImage = new BulkProcessImage();
    bulkImage.setLocation("stored/image1.jpg");
    bulkImage.setSequence(0);

    Map<String, BulkProcessImage> imageMap = Map.of("image1.jpg", bulkImage);

    ProductL3Response response = new ProductL3Response();
    MasterDataProductDTO masterData = new MasterDataProductDTO();
    masterData.setMasterDataProductImages(Collections.emptyList());
    response.setMasterDataProduct(masterData);

    List<ProductLevel3SummaryDetailsImageRequest> result = ResponseHelper.generateCommonImageRequestForEdit(response, request, imageMap, prefix,
        "");
    assertEquals(1, result.size());
    ProductLevel3SummaryDetailsImageRequest img = result.get(0);
    Assertions.assertTrue(img.getMainImage());
    assertEquals("new", img.getReviewType());
    Assertions.assertTrue(request.isImagesUpdated());
  }

  @Test
  void testBuildEditRequest_withNonYoutubeVideo() {
    BulkBasicInfoUpdateRequest request = BulkBasicInfoUpdateRequest.builder()
        .productSku("SKU123")
        .productName("Test Product")
        .description("Test Description")
        .inStore(true)
        .videoUrl("http://test.com/video.mp4")
        .shippingType("Dikirim oleh seller")
        .length(10.0)
        .width(5.0)
        .height(2.0)
        .weight(1.5)
        .shippingWeight(2.0)
        .sizeChartCode("SIZE123")
        .youtubeUrl(false) // non-youtube case
        .build();

    BulkProcessVideo bulkVideo = BulkProcessVideo.builder()
        .videoUrl("http://test.com/video.mp4")
        .videoName("Test Video")
        .videoId("VID123")
        .build();

    Map<String, BulkProcessVideo> videoMap = new HashMap<>();
    videoMap.put("http://test.com/video.mp4", bulkVideo);

    List<ProductLevel3SummaryDetailsImageRequest> images = new ArrayList<>();
    images.add(new ProductLevel3SummaryDetailsImageRequest());

    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setOfficial(true);
    profileResponse.setBopisFlag(true);
    profileResponse.setBigProductFlag(false);

    Set<L3InfoUpdateChangeType> changeTypes = EnumSet.of(L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE);

    ProductMasterDataEditRequest masterDataEditRequest = ProductMasterDataEditRequest.builder().build();

    // When
    ProductMasterDataEditRequest.ProductMasterDataEditRequestBuilder builder =
        ProductMasterDataEditRequest.builder();
    ProductMasterDataEditRequest builtRequest = builder.build();

    // Call the method under test

    ResponseHelper.buildEditRequest(request, videoMap, builtRequest, images, profileResponse, changeTypes, new ProductL3Response());
    assertEquals("SKU123", builtRequest.getProductSku());
    assertEquals(images, builtRequest.getProductLevel3SummaryDetailsImageRequests());
    Assertions.assertNotNull(builtRequest.getVideoAddEditRequest());
    assertEquals("Test Video", builtRequest.getVideoAddEditRequest().getVideoName());
    assertEquals("VID123", builtRequest.getVideoAddEditRequest().getVideoId());
    assertEquals("http://test.com/video.mp4", builtRequest.getVideoAddEditRequest().getVideoUrl());

    Assertions.assertTrue(builtRequest.getMasterDataEditChangeTypes().contains(L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE));
  }

  @Test
  void testBuildEditRequest_videoUrlBlank() {
    BulkBasicInfoUpdateRequest request = BulkBasicInfoUpdateRequest.builder()
        .productSku("SKU123")
        .productName("Test Product")
        .videoUrl("") // blank
        .youtubeUrl(true).weight(0.23)
        .build();

    Map<String, BulkProcessVideo> videoMap = new HashMap<>();

    ProductMasterDataEditRequest editRequest = ProductMasterDataEditRequest.builder().build();

    ResponseHelper.buildEditRequest(request, videoMap, editRequest, new ArrayList<>(), null, EnumSet.noneOf(L3InfoUpdateChangeType.class),new ProductL3Response() );

    Assertions.assertNull(editRequest.getUrl()); // because videoUrl is blank
    Assertions.assertNull(editRequest.getVideoAddEditRequest()); // no bulkVideo processed
  }


  @Test
  void testBuildEditRequest_youtubeVideo() {
    BulkBasicInfoUpdateRequest request = BulkBasicInfoUpdateRequest.builder()
        .videoUrl("http://youtube.com/video")
        .youtubeUrl(true).weight(0.23)
        .build();
    ProductMasterDataEditRequest editRequest = ProductMasterDataEditRequest.builder().build();
    ResponseHelper.buildEditRequest(request, Collections.emptyMap(), editRequest, new ArrayList<>(), new ProfileResponse(), EnumSet.noneOf(L3InfoUpdateChangeType.class),new ProductL3Response() );
    assertEquals("http://youtube.com/video", editRequest.getUrl());
    Assertions.assertNull(editRequest.getVideoAddEditRequest()); // not set because youtubeUrl
  }

  @Test
  void testBuildEditRequest_nonYoutubeVideo_bulkVideoNull() {
    BulkBasicInfoUpdateRequest request = BulkBasicInfoUpdateRequest.builder()
        .videoUrl("http://test.com/video.mp4")
        .youtubeUrl(false).shippingType("Dikirim oleh seller").weight(2300d)
        .build();
    Map<String, BulkProcessVideo> videoMap = new HashMap<>(); // empty map
    ProductMasterDataEditRequest editRequest = ProductMasterDataEditRequest.builder().build();
    ResponseHelper.buildEditRequest(request, videoMap, editRequest, new ArrayList<>(), new ProfileResponse(), EnumSet.noneOf(L3InfoUpdateChangeType.class),new ProductL3Response() );
    Assertions.assertNull(editRequest.getVideoAddEditRequest()); // video not found in map
    Assertions.assertEquals(2.3, editRequest.getWeight());
  }

  @Test
  void testCheckImageForErrors_CompletedWithError() {
    BulkProcessImage image = new BulkProcessImage();
    image.setCompleted(true);
    image.setErrorMessage("Error");
    assertTrue(ResponseHelper.checkImageForErrors(image));
  }

  @Test
  void testCheckImageForErrors_NotCompleted() {
    BulkProcessImage image = new BulkProcessImage();
    image.setCompleted(false);
    image.setErrorMessage("Error");
    assertFalse(ResponseHelper.checkImageForErrors(image));
  }

  @Test
  void testCheckImageForErrors_EmptyError() {
    BulkProcessImage image = new BulkProcessImage();
    image.setCompleted(true);
    image.setErrorMessage("");
    assertFalse(ResponseHelper.checkImageForErrors(image));
  }

  @Test
  void testCheckVideoForErrors_CompletedWithError() {
    BulkProcessVideo video = new BulkProcessVideo();
    video.setCompleted(true);
    video.setErrorMessage("Error");
    assertTrue(ResponseHelper.checkVideoForErrors(video));
  }

  @Test
  void testCheckVideoForErrors_NotCompleted() {
    BulkProcessVideo video = new BulkProcessVideo();
    video.setCompleted(false);
    video.setErrorMessage("Error");
    assertFalse(ResponseHelper.checkVideoForErrors(video));
  }

  @Test
  void testCheckVideoForErrors_EmptyError() {
    BulkProcessVideo video = new BulkProcessVideo();
    video.setCompleted(true);
    video.setErrorMessage("");
    assertFalse(ResponseHelper.checkVideoForErrors(video));
  }

  @Test
  void testBuildVideoUrlAndBulkProcessVideoMap() {
    BulkProcessVideo video1 = new BulkProcessVideo();
    video1.setUploadedURL("url1");

    BulkProcessVideo video2 = new BulkProcessVideo();
    video2.setUploadedURL("url2");

    BulkProcessVideo video3 = new BulkProcessVideo();
    video3.setUploadedURL("url1");

    List<BulkProcessVideo> videos = Arrays.asList(video1, video2, null, video3);
    Map<String, BulkProcessVideo> result = ResponseHelper.buildVideoUrlAndBulkProcessVideoMap(videos);

    assertEquals(2, result.size());
    assertEquals(video1, result.get("url1"));
    assertEquals(video2, result.get("url2"));
  }

  @Test
  void testGetBulkBasicInfoUpdateRequest() {
    Map<String, String> map = new HashMap<>();
    map.put(BulkParameters.MAIN_PHOTO, "img1");
    map.put(BulkParameters.COMMON_PHOTO_2, "img2");
    map.put(BulkParameters.COMMON_PHOTO_3, "");
    map.put(BulkParameters.COMMON_PHOTO_4, "img4");
    map.put(BulkParameters.COMMON_PHOTO_5, "");
    map.put(BulkParameters.COMMON_PHOTO_6, "img6");
    map.put(BulkParameters.COMMON_PHOTO_7, null);
    map.put(BulkParameters.COMMON_PHOTO_8, "img8");
    map.put(BulkParameters.VIDEO_URL, "https://youtube.com/sample");
    map.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU123");
    map.put(BulkParameters.PARENT_PRODUCT_NAME_HEADER, "Product");
    map.put(BulkParameters.INSTORE, "1");
    map.put(BulkParameters.BRAND_HEADER, "BrandX");
    map.put(BulkParameters.CATEGORY_HEADER, "CategoryA");
    map.put(BulkParameters.DESCRIPTIONS, "Desc");
    map.put(BulkParameters.SHIPPING_TYPE, "Standard");
    map.put(BulkParameters.LENGTH_HEADER, "10.0");
    map.put(BulkParameters.WIDTH_HEADER, "20.0");
    map.put(BulkParameters.HEIGHT_HEADER, "30.0");
    map.put(BulkParameters.ACTUAL_WEIGHT, "40.0");
    map.put(BulkParameters.SHIPPING_WEIGHT, "50.0");
    map.put(BulkParameters.SIZE_CHART, "SC123");

    BulkBasicInfoUpdateRequest req = ResponseHelper.getBulkBasicInfoUpdateRequest(map, "youtubeRegex");

    assertEquals(5, req.getCommonImages().size());
    assertEquals("SKU123", req.getProductSku());
    assertEquals("Product", req.getProductName());
    assertTrue(req.isInStore());
    assertEquals("BrandX", req.getBrand());
    assertEquals("CategoryA", req.getCategory());
    assertEquals("Desc", req.getDescription());
    assertEquals("Standard", req.getShippingType());
    assertEquals(10.0, req.getLength());
    assertEquals(20.0, req.getWidth());
    assertEquals(30.0, req.getHeight());
    assertEquals(40.0, req.getWeight());
    assertEquals(50.0, req.getShippingWeight());
    assertEquals("SC123", req.getSizeChartCode());
    assertEquals("img1", req.getMainImageUrl());
    assertTrue(req.isInStore());
    assertFalse(req.isYoutubeUrl());
  }

  @Test
  void testGetBulkBasicInfoUpdateRequest_NullAndEmptyDefaults() {
    Map<String, String> map = new HashMap<>();
    map.put(BulkParameters.INSTORE, "0");

    BulkBasicInfoUpdateRequest req = ResponseHelper.getBulkBasicInfoUpdateRequest(map, "");

    assertEquals(0.0, req.getLength());
    assertEquals(0.0, req.getWidth());
    assertEquals(0.0, req.getHeight());
    assertEquals(0.0, req.getWeight());
    assertEquals(0.0, req.getShippingWeight());
    assertFalse(req.isYoutubeUrl());
    assertFalse(req.isInStore());
  }

  @Test
  void testGetBulkBasicInfoUpdateRequest_NullAndEmptyDefaultsInvalidYoutube() {
    Map<String, String> map = new HashMap<>();
    map.put(BulkParameters.INSTORE, "0");
    map.put(BulkParameters.VIDEO_URL, "invalid_url");
    BulkBasicInfoUpdateRequest req = ResponseHelper.getBulkBasicInfoUpdateRequest(map, "");
    assertEquals(0.0, req.getLength());
    assertEquals(0.0, req.getWidth());
    assertEquals(0.0, req.getHeight());
    assertEquals(0.0, req.getWeight());
    assertEquals(0.0, req.getShippingWeight());
    assertFalse(req.isYoutubeUrl());
    assertFalse(req.isInStore());
  }

  @Test
  void testGetBulkBasicInfoUpdateRequest_NullAndEmptyDefaultsValidYoutube() {
    Map<String, String> map = new HashMap<>();
    map.put(BulkParameters.INSTORE, "0");
    map.put(BulkParameters.VIDEO_URL, "https://www.youtube.com/watch?v=du3Gig67kqk");
    BulkBasicInfoUpdateRequest req = ResponseHelper.getBulkBasicInfoUpdateRequest(map, "");
    assertEquals(0.0, req.getLength());
    assertEquals(0.0, req.getWidth());
    assertEquals(0.0, req.getHeight());
    assertEquals(0.0, req.getWeight());
    assertEquals(0.0, req.getShippingWeight());
    assertTrue(req.isYoutubeUrl());
    assertFalse(req.isInStore());
  }

  @Test
  void testGetErrorMessageForBulkProcessImage_WithError() {
    BulkProcessImage image = new BulkProcessImage();
    image.setErrorMessage("error msg");
    assertEquals("error msg", ResponseHelper.getErrorMessageForBulkProcessImage(image));
  }

  @Test
  void testGetErrorMessageForBulkProcessImage_Default() {
    assertEquals(BulkProcessValidationErrorMessages.IMAGE_VALIDATION_FAILED,
        ResponseHelper.getErrorMessageForBulkProcessImage(null));
    BulkProcessImage image = new BulkProcessImage();
    image.setErrorMessage("");
    assertEquals(BulkProcessValidationErrorMessages.IMAGE_VALIDATION_FAILED,
        ResponseHelper.getErrorMessageForBulkProcessImage(image));
  }

  @Test
  void testGetErrorMessageForBulkProcessVideo_WithError() {
    BulkProcessVideo video = new BulkProcessVideo();
    video.setErrorMessage("video error");
    assertEquals("video error", ResponseHelper.getErrorMessageForBulkProcessVideo(video));
  }

  @Test
  void testGetErrorMessageForBulkProcessVideo_Default() {
    assertEquals(BulkProcessValidationErrorMessages.VIDEO_VALIDATION_FAILED,
        ResponseHelper.getErrorMessageForBulkProcessVideo(null));
    BulkProcessVideo video = new BulkProcessVideo();
    video.setErrorMessage("");
    assertEquals(BulkProcessValidationErrorMessages.VIDEO_VALIDATION_FAILED,
        ResponseHelper.getErrorMessageForBulkProcessVideo(video));
  }

  @Test
  void testBulkImageIsNull_returnsTrue() {
    Map<String, MasterDataProductImageDTO> map = new HashMap<>();
    boolean result = ResponseHelper.isExistingImage(map, null, "image1.jpg");
    assertTrue(result);
  }

  @Test
  void testKeyExistsInMap_returnsTrue() {
    Map<String, MasterDataProductImageDTO> map = new HashMap<>();
    map.put("image1.jpg", new MasterDataProductImageDTO());
    BulkProcessImage bulkImage = new BulkProcessImage();
    boolean result = ResponseHelper.isExistingImage(map, bulkImage, "image1.jpg");
    assertTrue(result);
  }

  @Test
  void testKeyDoesNotExistAndBulkImageNotNull_returnsFalse() {
    Map<String, MasterDataProductImageDTO> map = new HashMap<>();
    BulkProcessImage bulkImage = new BulkProcessImage(); // not null
    boolean result = ResponseHelper.isExistingImage(map, bulkImage, "image2.jpg");
    assertFalse(result);
  }

}