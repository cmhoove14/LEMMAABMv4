% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Network_Mvmt_Functions.R
\name{get_contacts}
\alias{get_contacts}
\title{Simulate contacts}
\usage{
get_contacts(location, n_contacts, age, age_contacts_probs, agents_dt)
}
\arguments{
\item{location}{location of agent}

\item{n_contacts}{number of contacts drawn from NB distribution based on sociality metric and current time step's contact multiplier}

\item{age}{age of person}

\item{age_contacts_probs}{square matrix with dimensions equivalent to number of age classes encoding age-dependent contact probabilities}

\item{agents_dt}{data table of all agents}
}
\value{
agents in location who have infectious contact
}
\description{
Given infectious agent's location, age and their number of contacts, determine who is contacted in that location
}
