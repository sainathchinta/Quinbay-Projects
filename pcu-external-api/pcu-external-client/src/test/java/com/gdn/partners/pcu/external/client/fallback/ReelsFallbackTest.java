package com.gdn.partners.pcu.external.client.fallback;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.partners.pcu.external.client.helper.ReelsResponse;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.web.model.request.ReelsCreationRequest;
import com.gdn.partners.pcu.external.web.model.request.ReelsUpdateRequest;
import com.gdn.partners.pcu.external.web.model.response.ReelsListingResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class ReelsFallbackTest {

    private final ReelsFallback reelsFallback = new ReelsFallback();
    private static final int PAGE = 0;
    private static final int SIZE = 10;
    private static final String ORDER_BY = "createdDate";
    private static final String SORT_ORDER = "desc";
    private static final String OWNER_ID = "test-owner";

    @Test
    void getReelsListing_ShouldReturnFallbackResponse() {
        ReelsResponse<List<ReelsListingResponse>> response = reelsFallback.getReelsListing(
            PAGE, SIZE, ORDER_BY, SORT_ORDER, OWNER_ID);
        assertNotNull(response);
        assertEquals(HttpStatus.BAD_REQUEST.name(), response.getStatus());
        assertNotNull(response.getData());
        assertTrue(response.getData().isEmpty());
        assertNotNull(response.getErrors());
        assertEquals(1, response.getErrors().size());
        assertTrue(response.getErrors().containsKey(ErrorCategory.COMMUNICATION_FAILURE.getMessage()));
        assertEquals(
            Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE),
            response.getErrors().get(ErrorCategory.COMMUNICATION_FAILURE.getMessage())
        );
        assertNotNull(response.getMetadata());
        assertTrue(response.getMetadata().isEmpty());
        assertNull(response.getPaging());
    }

    @Test
    void getReelsListing_ShouldReturnSameResponseForDifferentParameters() {
        ReelsResponse<List<ReelsListingResponse>> response1 = reelsFallback.getReelsListing(
            0, 10, "createdDate", "desc", "owner1");
        ReelsResponse<List<ReelsListingResponse>> response2 = reelsFallback.getReelsListing(
            1, 20, "updatedDate", "asc", "owner2");
        assertEquals(response1.getStatus(), response2.getStatus());
        assertEquals(response1.getData(), response2.getData());
        assertEquals(response1.getErrors(), response2.getErrors());
        assertEquals(response1.getMetadata(), response2.getMetadata());
        assertEquals(response1.getPaging(), response2.getPaging());
    }

    @Test
    public void updateReelsProducts_shouldReturnBadRequestResponse() {
        ReelsUpdateRequest request = new ReelsUpdateRequest();
        request.setVideoId("video-1");
        request.setProductSkuList(List.of("sku-1", "sku-2"));
        String username = "test-user";
        ReelsResponse<Void> response = reelsFallback.updateReelsProducts(username, request);
        Assertions.assertNotNull(response);
    }

    @Test
    public void createNewReelsMapping_shouldReturnBadRequestResponse() {
        ReelsCreationRequest request = new ReelsCreationRequest();
        request.setVideoId("video-1");
        request.setProductSkuList(List.of("sku-1", "sku-2"));
        ReelsResponse<Void> response = reelsFallback.createNewReelsMapping(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void getVideoDetailsTest() {
        ReelsResponse<ReelsListingResponse> response =
            reelsFallback.getVideoDetails("reelId", "ownerId", 0);
        assertNotNull(response);
        assertEquals(HttpStatus.BAD_REQUEST.name(), response.getStatus());
    }

    @Test
    void deleteReelsTest() {
        ReelsResponse<Void> response =
          reelsFallback.deleteReels("reelId", "ownerId");
        assertNotNull(response);
        assertEquals(HttpStatus.BAD_REQUEST.name(), response.getStatus());
    }

}