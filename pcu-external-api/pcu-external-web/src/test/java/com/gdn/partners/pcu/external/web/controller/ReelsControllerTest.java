package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.SellerReelsApiPath;
import com.gdn.partners.pcu.external.service.ReelsService;
import com.gdn.partners.pcu.external.web.model.request.ReelsCreationRequest;
import com.gdn.partners.pcu.external.web.model.request.ReelsUpdateRequest;
import com.gdn.partners.pcu.external.web.model.response.ReelsListingResponse;
import com.gdn.partners.pcu.external.web.model.response.VideoDetailWebResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@ExtendWith(MockitoExtension.class)
class ReelsControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private ReelsController reelsController;

    @Mock
    private ReelsService reelsService;

    @Mock
    private MandatoryParameterHelper mandatoryParameterHelper;

    @Mock
    private ErrorController errorController;

    private static final String BUSINESS_PARTNER_CODE = "TEST_BP_CODE";
    private static final String REQUEST_ID = "test-request-id";
    private static final String VALID_SORT = "desc";
    private static final int DEFAULT_PAGE = 0;
    private static final int DEFAULT_SIZE = 25;
    private static final String API_PATH = SellerReelsApiPath.BASE_PATH + SellerReelsApiPath.LISTING;
    private static final String UPDATE = SellerReelsApiPath.BASE_PATH + SellerReelsApiPath.UPDATE;
    private static final String ADD = SellerReelsApiPath.BASE_PATH + SellerReelsApiPath.ADD_REEL;
    private static final String DELETE = SellerReelsApiPath.BASE_PATH + SellerReelsApiPath.DELETE;
    private static final String USERNAME = "test_user";
    private static final String VIDEO_ID = "video-id";

    @BeforeEach
    void setUp() {
        mockMvc = MockMvcBuilders.standaloneSetup(reelsController)
            .setControllerAdvice(errorController)
            .build();
    }

    @AfterEach
    public void tearDown() {
        Mockito.verifyNoMoreInteractions(reelsService);
        Mockito.verifyNoMoreInteractions(mandatoryParameterHelper);
    }

    @Test
    void getReelsListing_Success() throws Exception {
        when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
        when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
        ReelsListingResponse reel1 = new ReelsListingResponse();
        reel1.setVideoId("video1");
        reel1.setCaption("Test Reel 1");
        reel1.setCoverImagePath("path/to/cover1");
        reel1.setDurationInSeconds("30");

        ReelsListingResponse reel2 = new ReelsListingResponse();
        reel2.setVideoId("video2");
        reel2.setCaption("Test Reel 2");
        reel2.setCoverImagePath("path/to/cover2");
        reel2.setDurationInSeconds("45");

        Page<ReelsListingResponse> pageResponse = new PageImpl<>(
            Arrays.asList(reel1, reel2),
            PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE),
            2
        );

        when(reelsService.getReelsListing(
            BUSINESS_PARTNER_CODE,
            DEFAULT_PAGE,
            DEFAULT_SIZE,
            VALID_SORT
        )).thenReturn(pageResponse);

        mockMvc.perform(post(API_PATH)
                .param("page", String.valueOf(DEFAULT_PAGE))
                .param("size", String.valueOf(DEFAULT_SIZE))
                .param("sort", VALID_SORT)
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.success").value(true))
            .andExpect(jsonPath("$.requestId").value(REQUEST_ID))
            .andExpect(jsonPath("$.content[0].videoId").value("video1"))
            .andExpect(jsonPath("$.content[0].caption").value("Test Reel 1"))
            .andExpect(jsonPath("$.content[0].coverImagePath").value("path/to/cover1"))
            .andExpect(jsonPath("$.content[0].durationInSeconds").value("30"))
            .andExpect(jsonPath("$.content[1].videoId").value("video2"))
            .andExpect(jsonPath("$.content[1].caption").value("Test Reel 2"))
            .andExpect(jsonPath("$.content[1].coverImagePath").value("path/to/cover2"))
            .andExpect(jsonPath("$.content[1].durationInSeconds").value("45"))
            .andExpect(jsonPath("$.metadata.page").value(DEFAULT_PAGE))
            .andExpect(jsonPath("$.metadata.size").value(DEFAULT_SIZE));
    }

    @Test
    void getReelsListing_EmptyResponse() throws Exception {
        when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
        when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
        Page<ReelsListingResponse> emptyPage = new PageImpl<>(
            Collections.emptyList(),
            PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE),
            0
        );

        when(reelsService.getReelsListing(
            BUSINESS_PARTNER_CODE,
            DEFAULT_PAGE,
            DEFAULT_SIZE,
            VALID_SORT
        )).thenReturn(emptyPage);

        mockMvc.perform(post(API_PATH)
                .param("page", String.valueOf(DEFAULT_PAGE))
                .param("size", String.valueOf(DEFAULT_SIZE))
                .param("sort", VALID_SORT)
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.success").value(true))
            .andExpect(jsonPath("$.requestId").value(REQUEST_ID))
            .andExpect(jsonPath("$.content").isArray())
            .andExpect(jsonPath("$.content").isEmpty())
            .andExpect(jsonPath("$.metadata.page").value(DEFAULT_PAGE))
            .andExpect(jsonPath("$.metadata.size").value(DEFAULT_SIZE));
    }

    @Test
    void updateSellerReel_Success() throws Exception {
        ReflectionTestUtils.setField(reelsController, "reelsProductSkuUpdateMaxLimit", 10);
        when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
        when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
        ReelsUpdateRequest request = new ReelsUpdateRequest();
        request.setVideoId("video123");
        request.setProductSkuList(List.of("SKU1", "SKU2", "SKU3"));
        String requestJson =
            "{\"videoId\":\"video123\",\"productSkuList\":[\"SKU1\",\"SKU2\",\"SKU3\"]}";
        mockMvc.perform(post(UPDATE).content(requestJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true))
            .andExpect(jsonPath("$.requestId").value(REQUEST_ID));
        Mockito.verify(reelsService).updateReelsProducts(Mockito.any(), Mockito.any());
    }

    @Test
    void updateSellerReel_ExceedsproductSkuListLimit() throws Exception {
        int maxproductSkuListLimit = 2;
        ReflectionTestUtils.setField(reelsController, "reelsProductSkuUpdateMaxLimit", maxproductSkuListLimit);
        when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
        String requestJson = "{\"videoId\":\"video123\",\"productSkuList\":[\"SKU1\",\"SKU2\",\"SKU3\"]}";
        mockMvc.perform(post(UPDATE)
                .content(requestJson)
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is5xxServerError());
        verify(reelsService, never()).updateReelsProducts(any(), any());
        verify(mandatoryParameterHelper).getBusinessPartnerCode();
        verify(mandatoryParameterHelper).getRequestId();
    }

    @Test
    void updateSellerReel_NullproductSkuList() throws Exception {
        ReflectionTestUtils.setField(reelsController, "reelsProductSkuUpdateMaxLimit", 2);
        when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
        when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
        ReelsUpdateRequest request = new ReelsUpdateRequest();
        request.setVideoId("video123");
        request.setProductSkuList(List.of("SKU1", "SKU2", "SKU3"));
        String requestJson = "{\"videoId\":\"video123\",\"productSkuList\":[\"SKU1\",\"SKU2\",\"SKU3\"]}";
        mockMvc.perform(post(UPDATE).content(requestJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is5xxServerError());
        verify(reelsService, never()).updateReelsProducts(any(), any());
    }

    @Test
    void updateSellerReel_EmptyproductSkuList() throws Exception {
        ReflectionTestUtils.setField(reelsController, "reelsProductSkuUpdateMaxLimit", 10);
        when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
        when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
        ReelsUpdateRequest request = new ReelsUpdateRequest();
        request.setVideoId("video123");
        request.setProductSkuList(List.of("SKU1", "SKU2", "SKU3"));
        String requestJson = "{\"videoId\":\"video123\",\"productSkuList\":[]}";
        mockMvc.perform(post(UPDATE).content(requestJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is5xxServerError());
    }

    @Test
    void createNewReelsTest() throws Exception {
        ReflectionTestUtils.setField(reelsController, "reelsProductSkuUpdateMaxLimit", 10);
        when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
        when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
        ReelsCreationRequest request = new ReelsCreationRequest();
        request.setVideoId("video123");
        request.setProductSkuList(List.of("SKU1", "SKU2", "SKU3"));
        request.setOwnerId(BUSINESS_PARTNER_CODE);
        String requestJson =
          "{\"videoId\":\"video123\",\"productSkuList\":[\"SKU1\",\"SKU2\",\"SKU3\"]}";
        mockMvc.perform(post(ADD).content(requestJson).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true))
          .andExpect(jsonPath("$.requestId").value(REQUEST_ID));
        Mockito.verify(reelsService).createNewReelsMapping(request);
    }

    @Test
    void createNewReelsProductSkuListCrossedTest() throws Exception {
        ReflectionTestUtils.setField(reelsController, "reelsProductSkuUpdateMaxLimit", 1);
        when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
        when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
        ReelsCreationRequest request = new ReelsCreationRequest();
        request.setVideoId("video123");
        request.setProductSkuList(List.of("SKU1", "SKU2", "SKU3"));
        request.setOwnerId(BUSINESS_PARTNER_CODE);
        String requestJson =
          "{\"videoId\":\"video123\",\"productSkuList\":[\"SKU1\",\"SKU2\",\"SKU3\"]}";
        mockMvc.perform(post(ADD).content(requestJson).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().is5xxServerError());
        verify(reelsService, never()).updateReelsProducts(any(), any());
    }

    @Test
    void createNewReelsEmptyProductSkuListTest() throws Exception {
        ReflectionTestUtils.setField(reelsController, "reelsProductSkuUpdateMaxLimit", 1);
        when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
        when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
        ReelsCreationRequest request = new ReelsCreationRequest();
        request.setVideoId("video123");
        request.setOwnerId(BUSINESS_PARTNER_CODE);
        String requestJson =
          "{\"videoId\":\"video123\"}";
        mockMvc.perform(post(ADD).content(requestJson).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().is5xxServerError());
        verify(reelsService, never()).updateReelsProducts(any(), any());
    }


    @Test
    public void getVideoDetailsTest() throws Exception {
        Mockito.when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
        Mockito.when(mandatoryParameterHelper.getBusinessPartnerCode())
            .thenReturn(BUSINESS_PARTNER_CODE);
        Mockito.when(reelsService.getVideoDetails(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyInt())).thenReturn(new VideoDetailWebResponse());
        mockMvc.perform(
                get(SellerReelsApiPath.BASE_PATH + SellerReelsApiPath.DETAILS).param("videoId",
                        "testVideoId").param("businessPartnerCode", BUSINESS_PARTNER_CODE)
                    .param("videoSource", "0").contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true))
            .andExpect(jsonPath("$.requestId").value(REQUEST_ID));
        Mockito.verify(mandatoryParameterHelper).getRequestId();
        Mockito.verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }

    @Test
    void deleteReelsTest() throws Exception {
        when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
        when(mandatoryParameterHelper.getUsername()).thenReturn(USERNAME);
        when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
        mockMvc.perform(post(DELETE, VIDEO_ID).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true))
          .andExpect(jsonPath("$.requestId").value(REQUEST_ID));
        Mockito.verify(reelsService).deleteReels(VIDEO_ID, BUSINESS_PARTNER_CODE);
        Mockito.verify(mandatoryParameterHelper).getRequestId();
        Mockito.verify(mandatoryParameterHelper).getUsername();
        Mockito.verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }

    @Test
    void deleteReelsExceptionTest() throws Exception {
        when(mandatoryParameterHelper.getUsername()).thenReturn(USERNAME);
        when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
        Mockito.doThrow(RuntimeException.class).when(reelsService).deleteReels(VIDEO_ID, BUSINESS_PARTNER_CODE);
        mockMvc.perform(post(DELETE, VIDEO_ID).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().is5xxServerError());
        Mockito.verify(reelsService).deleteReels(VIDEO_ID, BUSINESS_PARTNER_CODE);
        Mockito.verify(mandatoryParameterHelper).getUsername();
        Mockito.verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
}