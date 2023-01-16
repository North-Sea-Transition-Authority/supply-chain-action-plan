package uk.co.nstauthority.scap.scap.summary;

import java.math.BigDecimal;
import java.util.List;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectType;

public record ProjectDetailsSummaryView(String projectName,
                                        List<ProjectType> projectTypes,
                                        BigDecimal projectCostEstimate,
                                        BigDecimal estimatedValueLocalContent,
                                        List<String> fieldNames,
                                        YesNo hasFacilities,
                                        List<String> projectFacilities,
                                        String plannedExecutionStartDate,
                                        String plannedCompletionDate) {
}
