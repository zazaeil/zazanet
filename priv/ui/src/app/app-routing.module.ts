import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { LogsViewerComponent } from './logs-viewer/logs-viewer.component';

const routes: Routes = [
    { path: '', component: LogsViewerComponent }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
