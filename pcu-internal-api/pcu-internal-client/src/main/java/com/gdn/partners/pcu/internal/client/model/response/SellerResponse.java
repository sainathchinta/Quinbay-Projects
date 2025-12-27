package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Objects;

/**
 * @author Navya Naveli
 */

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class SellerResponse {
    private String sellerName;
    private String sellerBadge;
    @JsonIgnore
    private Boolean isOfficial;

    @JsonProperty("official")
    public void setOfficial(Boolean official) {
        this.isOfficial = official;
    }

    @JsonProperty("isOfficial")
    public boolean getIsOfficial() {
        return Objects.nonNull(isOfficial) && isOfficial;
    }
}
