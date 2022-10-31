package uk.co.nstauthority.scap.energyportal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.tuple;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.client.field.FieldApi;
import uk.co.fivium.energyportalapi.generated.client.FieldsProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.Field;
import uk.co.fivium.energyportalapi.generated.types.FieldStatus;
import uk.co.nstauthority.scap.branding.CustomerConfigurationProperties;
import uk.co.nstauthority.scap.branding.ServiceBrandingConfigurationProperties;

@ExtendWith(MockitoExtension.class)
class FieldQueryServiceTest {

  private final ServiceBrandingConfigurationProperties serviceBrandingConfigurationProperties = new ServiceBrandingConfigurationProperties(
      new CustomerConfigurationProperties("name", "mnemonic", "guidanceDocumentUrl"),
      null
  );

  private FieldApi fieldApi;

  private FieldQueryService fieldQueryService;

  @BeforeEach
  void setup() {
    var energyPortalApiWrapper = new EnergyPortalApiWrapper(serviceBrandingConfigurationProperties);
    fieldApi = mock(FieldApi.class);
    fieldQueryService = new FieldQueryService(energyPortalApiWrapper, fieldApi);
  }

  @Test
  void getFieldSearchableResults() {
    var searchTerm = "test search term";
    var statuses = Arrays.stream(FieldStatus.values())
        .filter(fieldStatus -> fieldStatus != FieldStatus.STATUS9999)
        .toList();
    var field = new Field(13, "Field name", null, FieldStatus.STATUS050, null, null);

    when(fieldApi.searchFields(eq(searchTerm), eq(statuses), any(FieldsProjectionRoot.class), anyString(), anyString()))
        .thenReturn(List.of(field));

    var fieldSearchableResults = fieldQueryService.getFieldSearchableResults(searchTerm);

    assertThat(fieldSearchableResults)
        .extracting(
            FieldSelectable::getFieldId,
            FieldSelectable::getFieldName
        ).containsExactly(
            tuple(field.getFieldId().toString(), field.getFieldName())
        );
  }
}
