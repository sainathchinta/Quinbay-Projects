package com.gdn.partners.pcu.external.service.impl.helper;

import java.util.Objects;

import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.web.model.response.YouTubeAPIWebResponse;
import com.google.api.services.youtube.YouTube;
import com.google.api.services.youtube.model.VideoListResponse;

import lombok.extern.slf4j.Slf4j;

/**
 * Created by parvej on 13/08/2020 AD.
 */
@Slf4j
public class ValidateUrlUtil {

  public static YouTubeAPIWebResponse isYouTubeUrlActive(String videoID, String apiKey, YouTube youTube) {
    try {
      YouTube.Videos.List listVideosRequest = youTube.videos().list(Constants.VIDEO_LIST);
      listVideosRequest.setId(videoID);
      listVideosRequest.setKey(apiKey);
      VideoListResponse listResponse = listVideosRequest.execute();
      log.info("getting youtube api response: {}", listResponse);

      if (listResponse.getPageInfo().getTotalResults() >= 1 && Objects.nonNull(listResponse.getItems().get(0))) {
        return YouTubeAPIWebResponse.builder().isValid(true).validationSwitch(true)
            .description(listResponse.getItems().get(0).getSnippet().getDescription())
            .thumbnailImageUrl(listResponse.getItems().get(0).getSnippet().getThumbnails().getDefault().getUrl())
            .title(listResponse.getItems().get(0).getSnippet().getLocalized().getTitle()).build();
      }
    } catch (Exception e) {
      log.error("getting exception while calling youtube API for url: {} ", videoID, e);
    }
    return YouTubeAPIWebResponse.builder().isValid(false).validationSwitch(true).build();
  }
}
