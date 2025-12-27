package com.gdn.mta.bulk.util;

import com.gdn.partners.pbp.commons.constants.Constants;
import com.google.api.services.youtube.YouTube;
import com.google.api.services.youtube.model.VideoListResponse;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by parvej on 13/08/2020 AD.
 */
@Slf4j
public class ValidateUrlUtil {

  private static final String VIDEO_LIST = "snippet,contentDetails,statistics";
  private static final String YOUTUBE_REGEX = "^(http(s)?:\\/\\/)?((w){3}.)?youtu(be|.be)?(\\.com)?\\/.+";
  public static final Pattern YOUTUBE_PATTERN = Pattern.compile(YOUTUBE_REGEX);

  public static boolean validateYouTubeUrl(String youTubeUrl, String apiKey, YouTube youTube,
      boolean validateYoutubeUrlSwitchEn, String youtubeRegex) {
    try {
      String videoId = validateUrlAndGetVideoId(Pattern.compile(youtubeRegex), youTubeUrl);
      if (Objects.nonNull(videoId) && Objects.nonNull(validateUrlAndGetVideoId(YOUTUBE_PATTERN, youTubeUrl))) {
        log.info("Validating youtube videoId: {}", videoId);
        return !validateYoutubeUrlSwitchEn || isYouTubeUrlActive(videoId, apiKey, youTube, true);
      }
    } catch (Exception e) {
      log.error("getting exception while calling youtube API for url: {} ", youTubeUrl, e);
    }
    return false;
  }

  public static boolean validateYouTubeRegex(String youtubeUrl, String youtubeRegex) {
    String videoId = validateUrlAndGetVideoId(Pattern.compile(youtubeRegex), youtubeUrl);
    if (Objects.nonNull(videoId)) {
      return Objects.nonNull(validateUrlAndGetVideoId(YOUTUBE_PATTERN, youtubeUrl));
    }
    return false;
  }

  public static boolean isYouTubeUrlActive(String videoID, String apiKey, YouTube youTube,
      boolean validateYoutubeUrlSwitchEn) throws IOException {
    if (validateYoutubeUrlSwitchEn) {
      YouTube.Videos.List listVideosRequest = youTube.videos().list(VIDEO_LIST);
      listVideosRequest.setId(videoID);
      listVideosRequest.setKey(apiKey);
      VideoListResponse listResponse = listVideosRequest.execute();
      log.info("getting youtube api response: {}", listResponse);

      return listResponse.getPageInfo().getTotalResults() >= 1;
    }
    return true;
  }

  public static String validateUrlAndGetVideoId(Pattern pattern, String youtubeUrl) {
    Matcher matcher = pattern.matcher(youtubeUrl);
    int count = youtubeUrl.split(Constants.COMMA).length;
    if (count > 1) {
      return null;
    }
    if (matcher.find()) {
      return matcher.group();
    }
    return null;
  }
}
