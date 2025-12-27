package com.gdn.partners.pcu.internal.service.impl;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.internal.client.feign.MTAFeign;
import com.gdn.partners.pcu.internal.service.MTAService;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class MTAServiceImpl implements MTAService {

  private static final String CONTENT_REVIEWERS = "CONTENT_REVIEWERS";
  private static final String IMAGE_REVIEWERS = "IMAGE_REVIEWERS";
  private static final String TYPE_CONTENT_REVIEW = "content";
  private static final String TYPE_IMAGE_REVIEW = "image";

  @Autowired
  private MTAFeign mtaFeign;

  @Override
  public Map<String, List<String>> getImageOrContentReviewers(String reviewerType) {
    reviewerType = TYPE_CONTENT_REVIEW.equals(reviewerType) ? CONTENT_REVIEWERS : IMAGE_REVIEWERS;
    GdnRestSimpleResponse<Map<String, List<String>>> imageOrContentReviewers =
        mtaFeign.getImageOrContentReviewers(reviewerType);
    ResponseHelper.validateResponse(imageOrContentReviewers);
    return imageOrContentReviewers.getValue();
  }
}