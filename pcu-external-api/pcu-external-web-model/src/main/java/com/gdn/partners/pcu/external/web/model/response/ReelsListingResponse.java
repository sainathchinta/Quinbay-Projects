package com.gdn.partners.pcu.external.web.model.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ReelsListingResponse{
    private String videoId;
    private String coverImagePath;
    private String videoName;
    private String videoUrl;
    private List<String> productSkuList;
    private String caption;
    private Date uploadedTime;
    private String durationInSeconds;
    private ReelsAnalytics reelsAnalytics;
    private int ownerType;
} 