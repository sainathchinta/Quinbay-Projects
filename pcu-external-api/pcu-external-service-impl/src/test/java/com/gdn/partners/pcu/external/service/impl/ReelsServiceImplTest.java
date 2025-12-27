package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.external.client.feign.ReelsFeign;
import com.gdn.partners.pcu.external.client.feign.XProductFeign;
import com.gdn.partners.pcu.external.client.helper.Paging;
import com.gdn.partners.pcu.external.client.helper.ReelsResponse;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.partners.pcu.external.web.model.request.ReelsCreationRequest;
import com.gdn.partners.pcu.external.web.model.request.ReelsUpdateRequest;
import com.gdn.partners.pcu.external.web.model.response.ReelProductDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ReelsListingResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ReelsServiceImplTest {

  @Mock
  private ReelsFeign reelsFeign;

  @Mock
  private XProductFeign xProductFeign;

  @InjectMocks
  private ReelsServiceImpl reelsService;

  private static final String BUSINESS_PARTNER_CODE = "TEST_BP_CODE";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final String SORT = "desc";
  private static final String SUCCESS = "SUCCESS";
  private static final String USERNAME = "test_user";
  private static final String PRODUCT_SKU = "product-sku";
  private static final String VIDEO_ID = "video-id";

  private ReelsListingResponse reel1;
  private ReelsListingResponse reel2;

  @BeforeEach
  void setUp() {
    reel1 = new ReelsListingResponse();
    reel1.setVideoId("video1");
    reel1.setCaption("Test Reel 1");
    reel1.setCoverImagePath("path/to/cover1");
    reel1.setDurationInSeconds("30");
    reel1.setProductSkuList(List.of(PRODUCT_SKU));

    reel2 = new ReelsListingResponse();
    reel2.setVideoId("video2");
    reel2.setCaption("Test Reel 2");
    reel2.setCoverImagePath("path/to/cover2");
    reel2.setDurationInSeconds("45");
  }

  @Test
  void getReelsListing_Success() {
    List<ReelsListingResponse> reelsList = Arrays.asList(reel1, reel2);
    Paging paging = new Paging(PAGE, SIZE, 2L);
    ReelsResponse<List<ReelsListingResponse>> response =
        ReelsResponse.<List<ReelsListingResponse>>builder().status(SUCCESS).data(reelsList)
            .errors(null).metadata(null).paging(paging).build();

    when(reelsFeign.getReelsListing(eq(PAGE), eq(SIZE), eq(null), eq(SORT),
        eq(BUSINESS_PARTNER_CODE))).thenReturn(response);
    Page<ReelsListingResponse> result =
        reelsService.getReelsListing(BUSINESS_PARTNER_CODE, PAGE, SIZE, SORT);
    assertNotNull(result);
    assertEquals(2, result.getTotalElements());
    assertEquals(2, result.getContent().size());
    assertEquals(reel1.getVideoId(), result.getContent().get(0).getVideoId());
    assertEquals(reel2.getVideoId(), result.getContent().get(1).getVideoId());
    assertEquals(PageRequest.of(PAGE, SIZE), result.getPageable());
  }

  @Test
  void getReelsListing_EmptyResponse() {
    Paging paging = new Paging(PAGE, SIZE, 0L);
    ReelsResponse<List<ReelsListingResponse>> response =
        ReelsResponse.<List<ReelsListingResponse>>builder().status(SUCCESS)
            .data(Collections.emptyList()).errors(null).metadata(null).paging(paging).build();
    when(reelsFeign.getReelsListing(eq(PAGE), eq(SIZE), eq(null), eq(SORT),
        eq(BUSINESS_PARTNER_CODE))).thenReturn(response);
    Page<ReelsListingResponse> result =
        reelsService.getReelsListing(BUSINESS_PARTNER_CODE, PAGE, SIZE, SORT);
    assertNotNull(result);
    assertEquals(0, result.getTotalElements());
    assertTrue(result.getContent().isEmpty());
    assertEquals(PageRequest.of(PAGE, SIZE), result.getPageable());
  }

  @Test
  void getReelsListing_ErrorResponse() {
    ReelsResponse<List<ReelsListingResponse>> response =
        ReelsResponse.<List<ReelsListingResponse>>builder().status(HttpStatus.BAD_REQUEST.name())
            .data(null).errors(Map.of("error", Collections.singletonList("Error fetching reels")))
            .metadata(null).paging(null).build();
    when(reelsFeign.getReelsListing(eq(PAGE), eq(SIZE), eq(null), eq(SORT),
        eq(BUSINESS_PARTNER_CODE))).thenReturn(response);
    ClientException exception = assertThrows(ClientException.class,
        () -> reelsService.getReelsListing(BUSINESS_PARTNER_CODE, PAGE, SIZE, SORT));
    assertEquals("Error fetching reels", exception.getMessage());
  }

  @Test
  void getReelsListing_NullResponse() {
    when(reelsFeign.getReelsListing(eq(PAGE), eq(SIZE), eq(null), eq(SORT),
        eq(BUSINESS_PARTNER_CODE))).thenReturn(null);
    ClientException exception = assertThrows(ClientException.class,
        () -> reelsService.getReelsListing(BUSINESS_PARTNER_CODE, PAGE, SIZE, SORT));
    assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
  }

  @Test
  void getReelsListing_NullData() {
    Paging paging = new Paging(PAGE, SIZE, 0L);
    ReelsResponse<List<ReelsListingResponse>> response =
        ReelsResponse.<List<ReelsListingResponse>>builder().status(SUCCESS).data(null).errors(null)
            .metadata(null).paging(paging).build();
    when(reelsFeign.getReelsListing(eq(PAGE), eq(SIZE), eq(null), eq(SORT),
        eq(BUSINESS_PARTNER_CODE))).thenReturn(response);
    ClientException exception = assertThrows(ClientException.class,
        () -> reelsService.getReelsListing(BUSINESS_PARTNER_CODE, PAGE, SIZE, SORT));
    assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
  }

  @Test
  public void updateReelsProducts_Success() {
    // Given
    ReelsUpdateRequest request = new ReelsUpdateRequest();
    request.setVideoId("video123");
    request.setProductSkuList(Arrays.asList("SKU1", "SKU2", "SKU1", "SKU3")); // SKU1 is duplicated
    ReelsResponse<Void> mockResponse =
        ReelsResponse.<Void>builder().status(SUCCESS).data(null).errors(null).metadata(null)
            .paging(null).build();
    when(reelsFeign.updateReelsProducts(eq(USERNAME), any(ReelsUpdateRequest.class)))
        .thenReturn(mockResponse);
    reelsService.updateReelsProducts(request, USERNAME);
    List<String> expectedSkus = Arrays.asList("SKU1", "SKU2", "SKU3");
    verify(reelsFeign).updateReelsProducts(eq(USERNAME), eq(request));
    assert request.getProductSkuList().size() == 3;
    assert request.getProductSkuList().containsAll(expectedSkus);
  }

  @Test
  public void createNewReelsMappingTest() {
    ReelsCreationRequest reelsCreationRequest = new ReelsCreationRequest();
    reelsCreationRequest.setVideoId("video123");
    ReelsResponse<Void> mockResponse =
      ReelsResponse.<Void>builder().status(SUCCESS).data(null).errors(null).metadata(null)
        .paging(null).build();
    when(reelsFeign.createNewReelsMapping(any(ReelsCreationRequest.class))).thenReturn(
      mockResponse);
    reelsService.createNewReelsMapping(reelsCreationRequest);
    verify(reelsFeign).createNewReelsMapping(eq(reelsCreationRequest));
  }

  @Test
  void getVideoDetailsTest() {
    Mockito.when(
            reelsFeign.getVideoDetails(Mockito.anyString(), Mockito.anyString(), Mockito.anyInt()))
        .thenReturn(
            ReelsResponse.<ReelsListingResponse>builder().status(SUCCESS).data(reel1).errors(null)
                .metadata(null).paging(new Paging(0, 1, 1L)).build());
    Mockito.when(
            xProductFeign.getReelProductList(Mockito.anyInt(), Mockito.anyInt(), Mockito.any()))
        .thenReturn(new GdnRestListResponse<ReelProductDetailWebResponse>(null, null, true,
            Collections.singletonList(new ReelProductDetailWebResponse()), null, null));
    reelsService.getVideoDetails("video1", BUSINESS_PARTNER_CODE, 0);
    Mockito.verify(reelsFeign)
        .getVideoDetails(Mockito.anyString(), Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(xProductFeign)
        .getReelProductList(Mockito.anyInt(), Mockito.anyInt(), Mockito.any());
  }

  @Test
  public void deleteReelsMappingTest() {
    ReelsResponse<Void> mockResponse =
      ReelsResponse.<Void>builder().status(SUCCESS).data(null).errors(null).metadata(null)
        .paging(null).build();
    when(reelsFeign.deleteReels(VIDEO_ID, BUSINESS_PARTNER_CODE)).thenReturn(mockResponse);
    reelsService.deleteReels(VIDEO_ID, BUSINESS_PARTNER_CODE);
    verify(reelsFeign).deleteReels(eq(VIDEO_ID), eq(BUSINESS_PARTNER_CODE));
  }
}