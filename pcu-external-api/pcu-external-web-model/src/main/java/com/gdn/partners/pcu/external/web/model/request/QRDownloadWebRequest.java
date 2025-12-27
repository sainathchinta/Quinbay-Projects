package com.gdn.partners.pcu.external.web.model.request;

import java.util.ArrayList;
import java.util.List;

import com.gdn.partners.pcu.external.web.model.enums.TemplateSize;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class QRDownloadWebRequest {

  private String type;
  private boolean isProductLevel;
  private Boolean isDarkTheme;
  @NotBlank(message = "merchant code should not be blank")
  private String merchantCode;
  private TemplateSize templateSize;
  private List<QRProductWebRequest> products = new ArrayList<>();

}
