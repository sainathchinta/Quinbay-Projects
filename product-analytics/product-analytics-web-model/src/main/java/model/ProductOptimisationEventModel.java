package model;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class ProductOptimisationEventModel extends GdnBaseDomainEventModel implements Serializable {

  @Serial
  private static final long serialVersionUID = 2124385623446550970L;

  private String productSku;
  private String productName;
  private float productScore;
  private List<ProductOptimisationSuggestionsModel> suggestions = new ArrayList<>();
  private String sellerCode;
  private String categoryCode;
  private String categoryName;
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm:ss.SSS Z")
  private Date suggestedDate;
  private String imageUrl;
  private boolean markForDelete;
}
